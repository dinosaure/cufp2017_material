module Make
    (Store : Store.S with type Digest.t = Bytes.t
                      and type Digest.buffer = Cstruct.t
                      and type Hash.t = Bytes.t (* Digest.t: FIXME! *)
                      and type FileSystem.File.error = [ `System of string ]
                      and type FileSystem.File.raw = Cstruct.t
                      and type FileSystem.Dir.error = [ `System of string ]
                      and type FileSystem.Mapper.error = [ `System of string ]
                      and type FileSystem.Mapper.raw = Cstruct.t)
= struct
  module Digest = Store.Digest
  module Path = Store.Path
  module FileSystem = Store.FileSystem
  module Hash = Helper.BaseBytes
  module PACKDecoder = Store.PACKDecoder
  module PACKEncoder = Store.PACKEncoder

  type info =
    { max_length : int
    ; max_depth  : int }

  module Graph = Map.Make(Int64)

  let path_idx state hash_idx =
    let buf = Buffer.create (5 + Digest.length * 2) in
    let fmt = Format.formatter_of_buffer buf in

    Format.fprintf fmt "pack-%a%!" Hash.pp hash_idx;
    let filename_pack = Buffer.contents buf in

    Path.((Store.dotgit state / "objects" / "pack" / filename_pack) + "pack")

  let info_from_pack state ~raw ~ztmp ~window hash_idx =
    let path = path_idx state hash_idx in

    let open Lwt.Infix in

    FileSystem.File.open_r ~mode:0o644 ~lock:(Lwt.return ()) path >>= function
    | Error #FileSystem.File.error as err -> Lwt.return err
    | Ok fd ->
      let state = PACKDecoder.P.default ztmp window in

      let rec go length graph t = match PACKDecoder.P.eval raw t with
        | `Flush t ->
          let o, n = PACKDecoder.P.output t in
          go length graph (PACKDecoder.P.flush 0 (Cstruct.len o) t)
        | `Hunk (t, _) ->
          go length graph (PACKDecoder.P.continue t)
        | `Error (err, t) ->
          Lwt.return (Error (`PackDecoder err))
        | `Await t ->
          FileSystem.File.read raw ~off:0 ~len:(Cstruct.len raw) fd >>= (function
              | Ok n ->
                let t = PACKDecoder.P.refill 0 n t in
                go length graph t
              | Error #FileSystem.File.error as err -> Lwt.return err)
        | `End (t, hash) ->
          assert (Hash.equal hash hash_idx);

          Lwt.return (Ok { max_depth = Graph.fold (fun _ -> max) graph 0; max_length = length })
        | `Object t ->
          match PACKDecoder.P.kind t with
          | PACKDecoder.P.Commit
          | PACKDecoder.P.Tree
          | PACKDecoder.P.Tag
          | PACKDecoder.P.Blob ->
            go (max (PACKDecoder.P.length t) length) graph (PACKDecoder.P.next_object t)
          | PACKDecoder.P.Hunk ({ PACKDecoder.P.H.reference = PACKDecoder.P.H.Offset rel_off; _ } as hunks) ->
            let off = PACKDecoder.P.offset t in
            let length =
              max length
              @@ max (PACKDecoder.P.length t)
              @@ max hunks.PACKDecoder.P.H.target_length hunks.PACKDecoder.P.H.source_length
            in
            let graph =
              let depth_base =
                try Graph.find Int64.(sub off rel_off) graph
                with Not_found -> 0
              in

              Graph.add off (depth_base + 1) graph
            in

            go length graph (PACKDecoder.P.next_object t)
          | PACKDecoder.P.Hunk hunks ->
            let length =
              max length
              @@ max (PACKDecoder.P.length t)
              @@ max hunks.PACKDecoder.P.H.target_length hunks.PACKDecoder.P.H.source_length
            in
            go length graph (PACKDecoder.P.next_object t)
      in

      go 0 Graph.empty state

  let info state =
    let open Lwt.Infix in

    Lwt_list.fold_left_s
      (fun acc hash_idx -> info_from_pack state
          ~raw:(Store.buffer_io state)
          ~ztmp:(Store.buffer_zl state)
          ~window:(Store.buffer_window state)
          hash_idx
        >|= function
        | Error _ -> None
        | Ok { max_length; max_depth; } ->
          match acc with
          | None -> None (* XXX(dinosaure): any error occured invalids the expected information. *)
          | Some acc -> Some { max_length = max acc.max_length max_length
                             ; max_depth = max acc.max_depth max_depth })
      (Some { max_length = 0
            ; max_depth = 0})
      (Store.indexes state)

  let pack state =
    let open Lwt.Infix in

    info state >>= fun info ->
    Store.list state >>= fun lst ->
    let names = Hashtbl.create 0x100 in

    Lwt_list.iter_s (fun hash -> Store.read state hash )
end
