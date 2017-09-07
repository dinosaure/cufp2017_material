module Capabilities =
struct
  let default =
  [ `Multi_ack
  ; `Thin_pack
  ; `Side_band_64k
  ; `Ofs_delta
  ; `Agent "git/1.9.5"
  ; `Report_status ]
end

module Decoder = Smart_http.Decoder(Sha1)(Web_cohttp_lwt_jsoo)
module Encoder = Smart_http.Encoder(Sha1)(Web_cohttp_lwt_jsoo)
module Mem     = Mem.Make(Sha1)(Fpath)(Fs_lwt_jsoo.Lock)(Ocaml_inflate)(Ocaml_deflate)(Cstruct_buffer)
module Graph   = Object_graph.Make(Mem)

module PACKDecoder = Unpack.MakePACKDecoder(Sha1)(Ocaml_inflate)

let default_headers =
  Web_cohttp_lwt_jsoo.HTTP.Headers.empty

let root_uri = Uri.of_string "http://din.osau.re:8080/"
let repository = "digestif" 

let to_stream ?first = function
  | `Empty ->
    let consumed = ref false in
    fun () -> if !consumed then Lwt.return None else (consumed := true; Lwt.return first)
  | `Stream stream ->
    Lwt_stream.(append (of_list (match first with Some x -> [ x ] | None -> [])) stream)
    |> fun stream -> fun () -> Lwt_stream.get stream
  | `String s ->
    Lwt_stream.of_list (List.append (match first with Some x -> [ x ] | None -> []) [ s ])
    |> fun stream -> fun () -> Lwt_stream.get stream
  | `Strings lst ->
    Lwt_stream.of_list (List.append (match first with Some x -> [ x ] | None -> []) lst)
    |> fun stream -> fun () -> Lwt_stream.get stream

let cstruct_copy cs =
  let ln = Cstruct.len cs in
  let rs = Cstruct.create ln in

  Cstruct.blit cs 0 rs 0 ln;
  rs

let option_map a f = match a with Some a -> Some (f a) | None -> None

let apply hunks_decr hunks base raw =
  if Cstruct.len raw < hunks_decr.PACKDecoder.H.target_length
  then raise (Invalid_argument "apply");

  let target_length =
    List.fold_left
      (fun acc -> function
        | PACKDecoder.H.Insert insert ->
          Cstruct.blit insert 0 raw acc (Cstruct.len insert); acc + Cstruct.len insert
        | PACKDecoder.H.Copy (off, len) ->
          Cstruct.blit base off raw acc len; acc + len)
      0 hunks
  in

  if (target_length = hunks_decr.PACKDecoder.H.target_length)
  then Ok raw
  else Error "Bad undelta-ification"

let k2k x =
  let open PACKDecoder in
  match x with
  | Commit -> `Commit
  | Tree -> `Tree
  | Blob -> `Blob
  | Tag -> `Tag
  | _ -> assert false

module Revidx = Map.Make(Int64)

let populate git stream =
  let empty  = Cstruct.create 0 in
  let buffer = Cstruct_buffer.create 0x800 in
  let queue  = Queue.create () in

  let open Lwt.Infix in

  let rec go ~revidx ?(src = empty) ?ctx ?hunks state =
    match PACKDecoder.eval src state with
    | `Await state ->
      (stream () >>= function
        | Some s ->
          let src = Cstruct.of_string s in
          Lwt.return (`Jump (fun () -> (go[@tailcall]) ~revidx ~src (PACKDecoder.refill 0 (Cstruct.len src) state)))
        | None -> Lwt.return (`Error "PACK Err: expected input."))
    | `End (state, hash) ->
      Lwt.return `End
    | `Error (t, err) ->
      Lwt.return (`Error (Fmt.strf "PACK Err: %a." PACKDecoder.pp_error err))
    | `Flush state ->
      let o, n = PACKDecoder.output state in

      Cstruct_buffer.add buffer (Cstruct.sub o 0 n);
      Lwt.return (`Jump (fun () -> (go[@tailcall]) ~revidx ~src (PACKDecoder.flush 0 (Cstruct.len o) state)))
    | `Hunk (state, hunk) ->
      let hunks = match hunks, hunk with
        | Some hunks, PACKDecoder.H.Insert raw ->
          let raw' = cstruct_copy raw in
          (PACKDecoder.H.Insert raw' :: hunks)
        | Some hunks, hunk -> (hunk :: hunks)
        | None, PACKDecoder.H.Insert raw ->
          let raw' = cstruct_copy raw in 
          [ PACKDecoder.H.Insert raw' ]
        | None, hunk -> [ hunk ]
      in

      Lwt.return (`Jump (fun () -> (go[@tailcall]) ~revidx ~src ~hunks (PACKDecoder.continue state)))
    | `Object state ->
      (match PACKDecoder.kind state with
       | (PACKDecoder.Commit
         | PACKDecoder.Tag
         | PACKDecoder.Tree
         | PACKDecoder.Blob) as kind ->

         let raw = Cstruct_buffer.contents buffer|> Cstruct.of_string in
         Cstruct_buffer.clear buffer;

         Mem.write_inflated git ~kind:(k2k kind) raw >|= fun hash -> Some hash
       | PACKDecoder.Hunk hunks_descr ->
           let hunks = begin[@warning "-8"] let Some hunks = option_map hunks List.rev in hunks end in
           let res = Cstruct.create (hunks_descr.PACKDecoder.H.target_length) in
           let hash_base = match hunks_descr.PACKDecoder.H.reference with
             | PACKDecoder.H.Hash hash -> hash
             | PACKDecoder.H.Offset off ->
               let off = Int64.sub (PACKDecoder.offset state) off in
               Revidx.find off revidx
           in

           Mem.read_inflated git hash_base >>= function
           | None ->
             Queue.push (hunks_descr, hunks, res, hash_base) queue;
             Lwt.return None
           | Some (kind, raw) ->
             let base = Cstruct.of_string raw in

             match apply hunks_descr hunks base res with
             | Ok result -> Mem.write_inflated git ~kind result >|= fun hash -> Some hash
             | Error err -> Lwt.fail (Failure err))
      >>= fun hash_opt ->
      let revidx = match hash_opt with
        | Some hash ->
          Revidx.add (PACKDecoder.offset state) hash revidx
        | None -> revidx
      in

      Lwt.return (`Jump (fun () -> (go[@tailcall]) ~revidx ~src (PACKDecoder.next_object state)))
  in

  let rec gogo () = match Queue.pop queue with
    | exception Queue.Empty -> Lwt.return `End
    | (hunks_descr, hunks, res, hash_base) ->
      Mem.read_inflated git hash_base >>= function
      | None ->
        Queue.push (hunks_descr, hunks, res, hash_base) queue;
        Lwt.return (`Jump (fun () -> (gogo[@tailcall]) ()))
      | Some (kind, raw) ->
        let base = Cstruct.of_string raw in

        match apply hunks_descr hunks base res with
        | Ok result -> Mem.write_inflated git ~kind result >|= fun hash ->
          `Jump (fun () -> (gogo[@tailcall]) ())
        | Error err -> Lwt.fail (Failure err)
  in

  let ztmp = Cstruct.create 0x800 in
  let window = PACKDecoder.Inflate.window () in

  (* XXX(dinosaure): this hack is to avoid a stack-overflow in
     JS. Indeed, JS does not apply the tail-call optimization. *)
  let rec trampoline = function
    | `Jump jump ->
      let value = ref None in

      Lwt.on_success (jump ()) (fun v -> value := Some v);

      while !value = None
      do () done;

      let v = begin[@warning "-8"] let Some v = !value in v end in
      (trampoline[@tailcall]) v
    | `End -> Lwt.return (Ok ())
    | `Error err -> Lwt.return (Error err)
  in

  go ~revidx:Revidx.empty (PACKDecoder.default ztmp window)
  >>= trampoline
  >>= function
  | Error _ as err -> Lwt.return err
  | Ok () ->
    Queue.fold (fun acc x -> x :: acc) [] queue
    |> Lwt_list.fold_left_s (fun acc (_, _, _, hash) -> Mem.exists git hash >|= function true -> acc + 1 | false -> acc) 0
    >>= function
    | 0 -> Lwt.return (Ok ())
    | n -> gogo () >>= trampoline

type graph =
  < links : < source : Js.js_string Js.t Js.readonly_prop 
            ; target : Js.js_string Js.t Js.readonly_prop > Js.t Js.js_array Js.t Js.readonly_prop
  ; nodes : < group : int Js.readonly_prop
            ; id : Js.js_string Js.t Js.readonly_prop > Js.t Js.js_array Js.t Js.readonly_prop > Js.t

let make_graph : graph -> unit =
  fun graph -> Js.Unsafe.fun_call (Js.Unsafe.js_expr "makegraph") [| Js.Unsafe.inject graph |]

let main () =
  let open Lwt.Infix in

  Cohttp_lwt_xhr.Client.call
    ~headers:default_headers
    `GET
    (Uri.with_path root_uri (String.concat "/" [ "dev"; repository; "info"; "refs" ])
     |> fun uri -> Uri.add_query_param uri ("service", [ "git-upload-pack" ]))
  >>= fun (resp, body) ->
  let resp = Web_cohttp_lwt_jsoo.{ resp; body; } in
  Decoder.decode resp (Decoder.smart_reply ~service:"git-upload-pack" ()) >>= function
  | Error err -> Format.printf "Err: %s.\n%!" err; Lwt.return ()
  | Ok v ->
    let (head, _, _) = List.find (fun (hash, refname, _) -> Mem.Reference.(equal (of_string refname) head)) v.Decoder.refs in
    Format.printf "remote HEAD: %a.\n%!" (Fmt.hvbox Sha1.pp) head;
    let req =
      Web_cohttp_lwt_jsoo.Request.v `POST
        ~path:[ "dev"; repository; "git-upload-pack" ]
        Web_cohttp_lwt_jsoo.HTTP.Headers.(def content_type "application/x-git-upload-pack-request" empty)
        Web_cohttp_lwt_jsoo.Request.empty_body
      |> fun req -> Encoder.encode req
        (`UploadRequest { Encoder.want = head, []
                        ; have = []
                        ; capabilities = Capabilities.default })
    in

    let body, consumer = Lwt_stream.create () in

    let go (req : Web_cohttp_lwt_jsoo.req) =
      let `Stream producer = Web_cohttp_lwt_jsoo.Request.body req in
      producer (function Some (buf, off, len) -> consumer (Some (Cstruct.to_string (Bigarray.Array1.sub buf off len |> Cstruct.of_bigarray)))
                       (* XXX(dinosaure): again, we can optimize this part (the copy between string/bigstring). *)
                       | None -> consumer None)
    in

    go req >>= fun () ->
    Format.printf "Launch POST request.\n%!";

    Cohttp_lwt_xhr.Client.call
      ~headers:(Web_cohttp_lwt_jsoo.Request.headers req)
      ~body:(`Stream body)
      ~chunked:true (Web_cohttp_lwt_jsoo.Request.meth req) (Web_cohttp_lwt_jsoo.Request.with_uri root_uri req)
    >>= fun (resp, body) ->
    let resp = Web_cohttp_lwt_jsoo.{ resp; body; } in

    Lwt.return ()

let () = Lwt.async main

