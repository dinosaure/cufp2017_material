let result_bind f a = match a with
  | Ok x -> f x
  | Error err -> Error err

module Lock =
struct
  module H = Hashtbl.Make(struct type t = Fpath.t let equal = Fpath.equal let hash = Hashtbl.hash end)

  type key = H.key
  type elt = Lwt_mutex.t
  type t = elt H.t

  let locks = H.create 24

  let make locks' path =
    assert (locks == locks');
    
    try H.find locks path
    with Not_found ->
      let m = Lwt_mutex.create () in
      H.add locks path m;
      m

  let remove path =
    H.remove locks path |> Lwt.return (* weak table? *)

  let lock m = Lwt_mutex.lock m
  let unlock m = Lwt_mutex.unlock m |> Lwt.return

  let with_lock m f =
    match m with
    | None -> f ()
    | Some m -> Lwt_mutex.with_lock m f
end

module File =
struct
  type path = Fpath.t
  type lock = Lock.elt
  type error = [ `System of string ]

  let pp_error ppf (`System err) = Fmt.pf ppf "(`System %s)" err

  let exists path =
    (try Ok (Sys.file_exists (Fpath.to_string path))
     with exn -> Error (`System (Printexc.to_string exn)))
    |> Lwt.return

  let delete ?lock path =
    Lock.with_lock lock
    @@ fun () ->
    (try ignore @@ Sys.remove (Fpath.to_string path); Ok ()
     with exn -> Error (`System (Printexc.to_string exn)))
    |> Lwt.return

  let move ?lock a b = assert false (* TODO: not available in FsFake. *)

  let lock path = assert false

  type raw = Cstruct.t
  type 'a fd =
    | Read of in_channel
    | Write of out_channel
    constraint 'a = [< `Read | `Write ]

  let open_r ?lock path ~mode:_ : ([ `Read ] fd, error) result Lwt.t =
    Lock.with_lock lock
    @@ fun () ->
    (try let fd = open_in (Fpath.to_string path) in Ok (Read fd)
     with exn -> Error (`System (Printexc.to_string exn)))
    |> Lwt.return

  let open_w ?lock path ~mode:_ : ([ `Write ] fd, error) result Lwt.t =
    Lock.with_lock lock
    @@ fun () ->
    (try let fd = open_out (Fpath.to_string path) in Ok (Write fd)
     with exn -> Error (`System (Printexc.to_string exn)))
    |> Lwt.return

  let write
    : raw -> ?off:int -> ?len:int -> [> `Write ] fd -> (int, error) result Lwt.t
    = fun raw ?(off = 0) ?(len = Cstruct.len raw) (Write oc) ->
      (* XXX(dinosaure): Lwt_pool available in jsoo? *)

      let tmp = Bytes.create 0x100 in
      let iter cs =
        Cstruct.iter
          (fun cs -> let n = min (Cstruct.len cs) (Bytes.length tmp) in
            if n = 0 then None else Some n)
          (fun cs -> let n = min (Cstruct.len cs) (Bytes.length tmp) in
            Cstruct.blit_to_bytes cs 0 tmp 0 n;
            Bytes.unsafe_to_string tmp, 0, n)
          cs
      in

      let iter = iter (Cstruct.sub raw off len) in

      let rec go acc = match iter () with
        | None -> Lwt.return (Ok acc)
        | Some (tmp, off, len) ->
          match output_substring oc tmp off len with
          | () -> go (acc + len)
          | exception exn -> Lwt.return (Error (`System (Printexc.to_string exn)))
      in

      go 0

  let read
    : raw -> ?off:int -> ?len:int -> [> `Read ] fd -> (int, error) result Lwt.t
    = fun raw ?(off = 0) ?(len = Cstruct.len raw) (Read ic) ->
      let tmp = Bytes.create 0x100 in

      let rec go off = function
        | 0 -> Lwt.return (Ok len)
        | rest ->
          let n = min rest (Bytes.length tmp) in
          match input ic tmp 0 n with
          | 0 -> Lwt.return (Ok (len - rest))
          | w ->
            Cstruct.blit_from_bytes tmp 0 raw off w;
            go (off + w) (rest - w)
          | exception exn -> Lwt.return (Error (`System (Printexc.to_string exn)))
      in

      go off len

  let close = function
    | Read ic -> let () = close_in ic in Lwt.return (Ok ())
    | Write oc -> let () = close_out oc in Lwt.return (Ok ())

  let atomic_read path =
    try
      let ic = open_in (Fpath.to_string path) in
      let ln = in_channel_length ic in
      let tp = Bytes.create ln in
      let () = really_input ic tp 0 ln in
      let () = close_in ic in
      Lwt.return (Some (Cstruct.of_bytes tp))
    with _ -> Lwt.return None

  let atomic_write ?lock ?temp:_ path value =
    Lock.with_lock lock
    @@ fun () ->
    try
      let oc = open_out (Fpath.to_string path) in
      let tp = Cstruct.to_string value in
      let () = output_string oc tp in
      let () = close_out oc in
      Lwt.return (Ok ())
    with exn -> Lwt.return (Error (`System (Printexc.to_string exn)))

  let test_and_set ?lock ?temp path ~test ~set =
    let open Lwt.Infix in

    Lock.with_lock lock
    @@ fun () ->
    atomic_read path >>= fun v ->
    let equal = match test, v with
      | None, None -> true
      | Some x, Some y -> Cstruct.equal x y
      | _ -> false
    in

    (if not equal
     then Lwt.return (Ok false)
     else
       (match set with
        | None -> delete path
        | Some v -> atomic_write ?temp path v)
       >|= result_bind (fun () -> Ok true))
end

module type GAMMA =
sig
  val temp : Fpath.t
end

module Dir (Gamma : GAMMA) =
struct
  type path = Fpath.t
  type error = [ `System of string ]

  let pp_error ppf (`System err) = Fmt.pf ppf "(`System %s)" err

  let exists path = Lwt.return (Ok true)
  let create ?path:_ ?mode:_ path = Lwt.return (Ok true)
  let delete ?recurse path = Lwt.return (Ok ()) (* TODO *)
  let contents ?(dotfiles = false) ?(rel = false) dir =
    try
      let lst =
        Sys.readdir (Fpath.to_string dir)
        |> Array.to_list
        |> List.map (if rel then (fun x -> Fpath.v x) else (fun x -> Fpath.(dir / x)))
      in
      Lwt.return (Ok lst)
    with exn -> Lwt.return (Error (`System (Printexc.to_string exn)))

  let current () = Sys.getcwd () |> fun x -> Lwt.return (Ok (Fpath.v x))

  let temp () = Lwt.return Gamma.temp
end

module Mapper =
struct
  type fd = in_channel
  type raw = Cstruct.t
  type path = Fpath.t
  type error = [ `System of string ]

  let pp_error ppf (`System err) = Fmt.pf ppf "(`System %s)" err

  let openfile path =
    try let fd = open_in (Fpath.to_string path) in
      Lwt.return (Ok fd)
    with exn -> Lwt.return (Error (`System (Printexc.to_string exn)))

  let length fd =
    try let ln = in_channel_length fd in
      Lwt.return (Ok (Int64.of_int ln))
    with exn -> Lwt.return (Error (`System (Printexc.to_string exn)))

  let map fd ?(pos = 0L) ~share:_ len =
    let open Lwt.Infix in

    length fd >>= function
    | Error _ as err -> Lwt.return err
    | Ok ln ->
      let ln = min Int64.(to_int (sub ln pos)) len in
      try
        let () = seek_in fd (Int64.to_int pos) in
        let tp = Bytes.create ln in
        match really_input fd tp 0 ln with
        | () -> Lwt.return (Ok (Cstruct.of_bytes tp))
        | exception exn -> Lwt.return (Error (`System (Printexc.to_string exn)))
      with exn ->
        Lwt.return (Error (`System (Printexc.to_string exn)))

  let close fd =
    try let () = close_in fd in Lwt.return (Ok ())
    with exn -> Lwt.return (Error (`System (Printexc.to_string exn)))
end

module Make (Gamma : GAMMA)
  : Fs.S with type path = Fpath.t
          and type error = [ `System of string ]
          and type File.path = Fpath.t
          and type File.lock = Lock.elt
          and type File.error = [ `System of string ]
          and type File.raw = Cstruct.t
          and type Dir.path = Fpath.t
          and type Dir.error = [ `System of string ]
          and type Mapper.raw = Cstruct.t
          and type Mapper.path = Fpath.t
          and type Mapper.error = [ `System of string ]
= struct
  type path = Fpath.t

  type error = [ `System of string ]

  let pp_error ppf (`System err) = Fmt.pf ppf "(`System %s)" err

  let is_file path =
    (try Ok (not (Sys.is_directory (Fpath.to_string path)))
    with exn -> Error (`System (Printexc.to_string exn)))
    |> Lwt.return

  let is_dir path =
    (try Ok (Sys.is_directory (Fpath.to_string path))
    with exn -> Error (`System (Printexc.to_string exn)))
    |> Lwt.return

  module File = File
  module Dir = Dir(Gamma)
  module Mapper = Mapper
end
