(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let ppe ~name ppv =
  Fmt.braces (fun ppf -> Fmt.pf ppf "%s %a" name (Fmt.hvbox ppv))

module BaseBigstring
  : S.BASE with type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t =
struct
  open Bigarray

  type t = (char, int8_unsigned_elt, c_layout) Array1.t

  let length x = Array1.dim x

  exception Break of int

  external uget : t -> int -> int = "%caml_ba_ref_1"

  let compare a b =
    if length a <> length b
    then 0
    else
      try
        for i = 0 to length a - 1
        do if uget a i <> uget b i then raise (Break ((uget a i) - (uget b i))) done;
        0
      with Break n -> n

  let equal a b = compare a b = 0

  let hash = Hashtbl.hash

  let pp ppf ba =
    for i = 0 to length ba - 1
    do Fmt.pf ppf "%02x" (uget ba i) done

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end

module BaseBytes :
sig
  include S.BASE with type t = Bytes.t

  val to_hex : t -> string
  val of_hex : string -> t
end = struct
  open Bytes

  type nonrec t = t

  let equal = equal
  let compare = compare
  let hash = Hashtbl.hash

  let pp = Fmt.iter Bytes.iter (Fmt.using Char.code (fun ppf -> Fmt.pf ppf "%02x"))

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)

  let to_hex x =
    let `Hex v = Hex.of_string (Bytes.unsafe_to_string x) in v
  let of_hex x =
    Hex.to_string (`Hex x) |> Bytes.unsafe_of_string
end

module MakeDecoder (A : S.ANGSTROM)
  : S.DECODER with type t = A.t
               and type raw = Cstruct.t
               and type init = Cstruct.t
               and type error = [ `Decoder of string ]
= struct
  (* XXX(dinosaure): this decoder is on top of some assertion about the decoding
     of a git object. [Angstrom] can not consume all input (when we have an
     alteration specifically) and the client need to keep a part of the previous
     input but continue to feed the parser with a new input (to determine the
     choice of the alteration). In the git format, we can have this problem but for
     few bytes (something like ten bytes). So, we consider to use an internal
     buffer (than the size is bigger than what is needed) and we ensure than is not
     possible to keep more than the size of the internal buffer when we parse a Git
     object.

     However, this case appear in the code (when we don't have enough trailing
     space and writable space). This case appear when we don't parse a Git
     object but something else (which is not respect our assertions). And if
     this case appear, we can consider that someone try to cheat you. So we
     prefer to return an error.

     Obviously, in a better world with unicorn and pineapple pizza, we can just
     avoid this case. *)

  type error = [ `Decoder of string ]
  type init = Cstruct.t
  type raw = Cstruct.t
  type t = A.t

  let pp_error ppf (`Decoder err) = ppe ~name:"`Decoder" Fmt.string ppf err

  type decoder =
    { state    : Angstrom.bigstring -> Angstrom.Unbuffered.more -> A.t Angstrom.Unbuffered.state
    ; final    : Angstrom.Unbuffered.more
    ; internal : Cstruct.t
    ; max      : int }

  let kdone (consumed, value) =
    fun _ _ -> Angstrom.Unbuffered.Done (consumed, value)

  let kfail (consumed, path, err) =
    fun _ _ -> Angstrom.Unbuffered.Fail (consumed, path, err)

  let default raw =
    if Cstruct.len raw < 25 * 2
    then raise (Invalid_argument "The internal buffer is not enough to parse");
    (* XXX(dinosaure): So, if you read the comment before, you understand than
       the internal buffer need to be bigger than an alteration. I decide
       (because I'm the God) to accept only a buffer bigger than 25 bytes.
       Otherwise, we raise an [Invalid_argument]. *)

    let len = Cstruct.len raw in

    { state = (match Angstrom.Unbuffered.parse A.decoder with
          | Angstrom.Unbuffered.Done (committed, value) ->
            kdone (committed, value)
          | Angstrom.Unbuffered.Fail (committed, path, err) ->
            kfail (committed, path, err)
          | Angstrom.Unbuffered.Jump jump -> fun _ _ -> jump () (* XXX(dinosaure): safe? *)
          | Angstrom.Unbuffered.Partial { Angstrom.Unbuffered.committed; continue; } ->
            assert (committed = 0);
            continue)
    ; final = Angstrom.Unbuffered.Incomplete
    ; internal = Cstruct.sub raw 0 0
    (* XXX(dinosaure): first, we consider than the internal buffer is empty. But
       keep calm, the physical buffer will not be free-ed. *)
    ; max      = len }

  let rec eval decoder =
    match decoder.state (Cstruct.to_bigarray decoder.internal) decoder.final with
    | Angstrom.Unbuffered.Done (consumed, value) -> `End (Cstruct.shift decoder.internal consumed, value)
    | Angstrom.Unbuffered.Jump jump as x ->
      let rec go = function
        | Angstrom.Unbuffered.Jump jump -> (go[@tailcall]) (jump ())
        | Angstrom.Unbuffered.Done (committed, value) ->
          kdone (committed, value)
        | Angstrom.Unbuffered.Fail (committed, path, err) ->
          kfail (committed, path, err)
        | Angstrom.Unbuffered.Partial { Angstrom.Unbuffered.committed; continue; } ->
          continue
      in

      let state = go x in (eval[@tailcall]) { decoder with state }
    | Angstrom.Unbuffered.Fail (consumed, path, err) ->
      `Error (Cstruct.shift decoder.internal consumed, `Decoder (String.concat " > " path ^ ": " ^ err))
    | Angstrom.Unbuffered.Partial
        { Angstrom.Unbuffered.committed; continue; } ->
      let decoder = { decoder with internal = Cstruct.shift decoder.internal committed
                                 ; state = continue } in
      `Await decoder

  let compress input decoder =
    let off, len = 0, Cstruct.len decoder.internal in
    let buffer = Cstruct.of_bigarray ~off ~len decoder.internal.Cstruct.buffer in
    Cstruct.blit decoder.internal 0 buffer 0 len;
    { decoder with internal = buffer }

  let refill input decoder =
    let len = Cstruct.len input in

    if len > decoder.max
    then
      Error (`Decoder (Fmt.strf "Input is too huge: we authorized only an \
                                 input lower or equal than %d" decoder.max))
    else

      let _trailing_space =
        let { Cstruct.buffer; off; len } = decoder.internal in
        Bigarray.Array1.dim buffer - (off + len)
      in

      let _writable_space =
        let { Cstruct.buffer; off; len } = decoder.internal in
        Bigarray.Array1.dim buffer - len
      in

      if _trailing_space >= len
      then Ok decoder
      else if _writable_space >= len
      then Ok (compress input decoder)
      else Error (`Decoder "Input does not respect assertion, it may be malicious");

      |> function Error err -> Error err
                | Ok decoder ->
                  let internal = Cstruct.add_len decoder.internal len in

                  let off = Cstruct.len internal - len in
                  let ()  = Cstruct.blit input 0 internal off len in

                  Ok { decoder with internal = internal }

  let to_result input =
    Angstrom.parse_bigstring A.decoder (Cstruct.to_bigarray input)
    |> function Ok v -> Ok v
              | Error err -> Error (`Decoder err)

  let finish decoder =
    { decoder with final = Angstrom.Unbuffered.Complete }
end

module MakeInflater (Z : S.INFLATE) (A : S.ANGSTROM)
  : S.DECODER with type t = A.t
                    and type raw = Cstruct.t
                    and type init = Z.window * Cstruct.t * Cstruct.t
                    and type error = [ `Decoder of string | `Inflate of Z.error ]
=
struct
  module D = MakeDecoder(A)

  type t = A.t
  type raw = Cstruct.t
  type init = Z.window * Cstruct.t * Cstruct.t
  type error =
    [ `Decoder of string
    | `Inflate of Z.error ]
  type decoder =
    { cur : Cstruct.t
    ; tmp : Cstruct.t
    ; inf : Z.t
    ; dec : D.decoder }

  let pp_error ppf = function
    | `Decoder err ->
      ppe ~name:"`Decoder" Fmt.string ppf err
    | `Inflate err ->
      ppe ~name:"`Inflate" (Fmt.hvbox Z.pp_error) ppf err

  let default (window, raw0, raw1) =
    { cur = Cstruct.sub raw0 0 0
    ; tmp = raw0
    ; inf = Z.flush 0 (Cstruct.len raw0)
            @@ Z.default (Z.window_reset window)
    ; dec = D.default raw1 }

  let rec eval decoder = match D.eval decoder.dec with
    | `Await dec ->
      (match Z.eval ~src:decoder.cur ~dst:decoder.tmp decoder.inf with
       | `Await inf ->
         `Await { decoder with cur = Cstruct.shift decoder.cur (Z.used_in inf)
                             ; inf = inf
                             ; dec = dec }
       | `Flush inf ->
         (match D.refill (Cstruct.sub decoder.tmp 0 (Z.used_out inf)) dec with
          | Ok dec ->
            let inf = Z.flush 0 (Cstruct.len decoder.tmp) inf in

            eval { decoder with dec = dec
                              ; inf = inf }
          | Error (`Decoder err) -> `Error (decoder.cur, `Decoder err))
       | `Error (inf, err) ->
         `Error (Cstruct.shift decoder.cur (Z.used_in inf), `Inflate err)
       | `End inf ->
         (match D.refill (Cstruct.sub decoder.tmp 0 (Z.used_out inf)) dec with
          | Ok dec -> eval { decoder with cur = Cstruct.shift decoder.cur (Z.used_in inf)
                                        ; inf = Z.flush 0 0 inf
                                        ; dec = D.finish dec }
          | Error (`Decoder err) -> `Error (decoder.cur, `Decoder err)))
    | `Error (unconsumed, `Decoder err) ->
      `Error (unconsumed, `Decoder err)
    | `End (unconsumed, value) ->
      `End (unconsumed, value)

  let refill input decoder =
    Ok { decoder with cur = input
                    ; inf = Z.refill 0 (Cstruct.len input) decoder.inf }

  let finish decoder =
    { decoder with dec = D.finish decoder.dec }

  let to_result x = assert false
end

module MakeEncoder (M : S.MINIENC)
  : S.ENCODER with type t = M.t
                    and type raw = Cstruct.t
                    and type init = int * M.t
                    and type error = [ `Never ]
= struct
  type t = M.t
  type raw = Cstruct.t
  type init = int * t
  type error = [ `Never ]

  let pp_error ppf `Never = Fmt.string ppf "`Never"

  type encoder =
      { o_off : int
      ; o_pos : int
      ; o_len : int
      ; w_acc : int
      ; state : Minienc.encoder Minienc.state }

  let default (capacity, x) =
    let encoder = Minienc.create capacity in
    let state   = M.encoder x (fun encoder -> Minienc.End encoder) encoder in
    { o_off = 0
    ; o_pos = 0
    ; o_len = 0
    ; w_acc = 0
    ; state }

  let cstruct_blit_iovec iovec cs cs_off limit =
    let max = fun len -> match limit with Some limit -> min limit len | None -> len in

    match iovec with
      | { Minienc.IOVec.buffer = `Bigstring x; off; len; } ->
        Cstruct.blit (Cstruct.of_bigarray ~off ~len x) 0 cs cs_off (max len); max len
      | { Minienc.IOVec.buffer = `Bytes x; off; len; } ->
        Cstruct.blit_from_bytes x off cs cs_off (max len); max len
      | { Minienc.IOVec.buffer = `String x; off; len; } ->
        Cstruct.blit_from_string x off cs cs_off (max len); max len

  exception Drain of int

  let rec eval current e = match e.state with
    | Minienc.End minienc ->
      let shift  = min (e.o_len - e.o_pos) (Minienc.has minienc) in
      let iovecs, shifted = Minienc.shift shift minienc in
      let shift' = List.fold_left (fun acc iovec ->
          let write = cstruct_blit_iovec iovec current acc None in
          acc + write)
          (e.o_off + e.o_pos) iovecs
      in

      assert (e.o_off + e.o_pos + shift = shift');

      if Minienc.has shifted > 0
      then `Flush { e with o_pos = e.o_pos + shift
                         ; w_acc = e.w_acc + shift
                         ; state = Minienc.End shifted }
      else `End ({ e with o_pos = e.o_pos + shift
                        ; w_acc = e.w_acc + shift
                        ; state = Minienc.End shifted }, e.w_acc + shift)
    | Minienc.Continue { encoder; continue; } ->
      (* XXX(dinosaure): we can shift the minienc at this time, but it's very useful? *)
      eval current { e with state = continue encoder }
    | Minienc.Flush { continue; iovecs; } ->
      let max = min (e.o_len - e.o_pos) (Minienc.IOVec.lengthv iovecs) in
      try
        let pos = ref e.o_pos in

        List.iter (fun iovec ->
            let write = cstruct_blit_iovec iovec current (e.o_off + !pos) (Some (e.o_len - !pos)) in

            if !pos + write = max
            then raise (Drain !pos)
            else pos := !pos + write)
          iovecs;

        raise (Drain !pos)
      with Drain drain ->
        `Flush { e with o_pos = e.o_pos + drain
                      ; w_acc = e.w_acc + drain
                      ; state = continue drain }

  let flush off len e =
    { e with o_off = off
           ; o_pos = 0
           ; o_len = len }

  let used { o_pos; _ } = o_pos
end

module MakeDeflater (Z : S.DEFLATE) (M : S.MINIENC)
  : S.ENCODER with type t = M.t
                    and type raw = Cstruct.t
                    and type init = int * M.t * int * Cstruct.t
                    and type error = [ `Deflate of Z.error ]
= struct
  type t = M.t
  type raw = Cstruct.t
  type init = int * t * int * Cstruct.t
  type error = [ `Deflate of Z.error ]

  let pp_error ppf (`Deflate err) =
    ppe ~name:"`Deflate" (Fmt.hvbox Z.pp_error) ppf err

  module E = MakeEncoder(M)

  type encoder =
    { e        : E.encoder
    ; z        : Z.t
    ; internal : Cstruct.t
    ; used_in  : int }

  let default (capacity, value, level, internal) =
    { e = E.default (capacity, value)
    ; z = Z.default level
    ; internal
    ; used_in = 0 }

  let rec eval dst encoder =
    match Z.eval ~src:encoder.internal ~dst encoder.z with
    | `Await z ->
      (match E.eval encoder.internal encoder.e with
       | `Flush e ->
         let used_in' = encoder.used_in + Z.used_in z in
         let z, e, used_in =
           if used_in' = E.used e
           then Z.no_flush 0 0 z, E.flush 0 (Cstruct.len encoder.internal) e, 0
           else Z.no_flush used_in' (E.used e - used_in') z, e, used_in'
         in

         eval dst { encoder with e; z; used_in; }
       | `End (e, _) ->
         let used_in' = encoder.used_in + Z.used_in z in
         let z, e, used_in =
           if used_in' = E.used e
           then Z.finish z, e, used_in'
           else Z.no_flush used_in' (E.used e - used_in') z, e, used_in'
         in

         eval dst { encoder with e; z; used_in; }
       | `Error `Never -> assert false)
    (* XXX(dinosaure): [`Never] is a trick just to constraint the type
                       error to be a polymorphic variant. But this case
                       never happens. *)
    | `Flush z ->
      `Flush { encoder with z; }
    | `End z ->
      `End ({ encoder with z; }, 0)
    | `Error (z, exn) -> `Error (`Deflate exn)

  let flush off len encoder =
    { encoder with z = Z.flush off len encoder.z }

  let used { z; _ } =
    Z.used_out z
end

let fdigest
  : type t hash. (module Ihash.IDIGEST with type t = hash
                                        and type buffer = Cstruct.t)
    -> (module S.ENCODER with type t = t
                               and type raw = Cstruct.t
                               and type init = (int * t)
                               and type error = [ `Never ])
    -> ?capacity:int
    -> tmp:Cstruct.t
    -> kind:string
    -> length:(t -> int64)
    -> t
    -> hash
  = fun digest encoder ?(capacity = 0x100) ~tmp ~kind ~length value ->
    let module Digest = (val digest) in
    let module M      = (val encoder) in

    let hdr = Fmt.strf "%s %Ld\000" kind (length value) in

    let ctx = Digest.init () in

    Digest.feed ctx (Cstruct.of_string hdr);
    let encoder = M.default (capacity, value) in

    let rec loop encoder = match M.eval tmp encoder with
      | `Flush encoder ->
        if M.used encoder > 0
        then begin
          Digest.feed ctx (Cstruct.sub tmp 0 (M.used encoder))
        end;

        loop (M.flush 0 (Cstruct.len tmp) encoder)
      | `End (encoder, _) ->
        if M.used encoder > 0
        then begin
          Digest.feed ctx (Cstruct.sub tmp 0 (M.used encoder))
        end;

        Digest.get ctx
      | `Error `Never -> assert false
    in

    loop encoder

let digest
  : type t hash. (module Ihash.IDIGEST with type t = hash
                                        and type buffer = Cstruct.t)
    -> (module S.FARADAY with type t = t)
    -> kind:string
    -> t
    -> hash
  = fun digest faraday ~kind value ->
    let module Digest = (val digest) in
    let module F      = (val faraday) in

    let hdr = Fmt.strf "%s %Ld\000" kind (F.length value) in
    let ctx = Digest.init () in
    let raw = Cstruct.create (Int64.to_int (F.length value)) in (* XXX(dinosaure): to digest. *)
    let enc = Faraday.of_bigstring (Cstruct.to_bigarray raw) in

    Digest.feed ctx (Cstruct.of_string hdr);

    F.encoder enc value;
    Faraday.close enc;
    Faraday.serialize enc
      (fun iovecs ->
          let len = List.fold_left (fun acc -> function
              | { Faraday.buffer = `String buf; off; len; } ->
                Digest.feed ctx (Cstruct.of_string (String.sub buf off len)); acc + len
              | { Faraday.buffer = `Bytes buf; off; len; } ->
                Digest.feed ctx (Cstruct.of_bytes (Bytes.sub buf off len)); acc + len
              | { Faraday.buffer = `Bigstring buf; off; len; } ->
                Digest.feed ctx (Cstruct.of_bigarray ~off ~len buf); acc + len)
              0 iovecs
          in `Ok len)
    |> function `Close -> Digest.get ctx
              | `Yield -> assert false

module type ENCODER =
sig
  type state
  type raw
  type result
  type error

  val raw_length : raw -> int
  val raw_blit   : raw -> int -> raw -> int -> int -> unit

  val eval  : raw -> state -> [ `Flush of state | `End of (state * result) | `Error of (state * error) ] Lwt.t
  val used  : state -> int
  val flush : int -> int -> state -> state
end

type ('state, 'raw, 'result, 'error) encoder =
  (module ENCODER with type state  = 'state
                   and type raw    = 'raw
                   and type result = 'result
                   and type error  = 'error)
and ('fd, 'raw, 'error) writer = 'raw -> ?off:int -> ?len:int -> 'fd -> (int, 'error) result Lwt.t

(* XXX(dinosaure): this function takes care about how many byte(s) we write to
   the file-descriptor and retry to write while the limit is not retrieved. In
   some case (but not common), the writer does not write all bytes available by
   the encoder. In this case, we compress the internal buffer [raw] and retry to
   write (and, at the same time, feed the rest of the internal buffer).

   If the internal buffer is full, we start to count how many call we need to
   write something and if we arrive to the limit before writing would be only
   one byte, we stop (but not close the file-descriptor) the process and return
   [`Stack].

   If we arrive at the end and the buffer is not empty, we count how many call
   we need to flush the buffer, and, as below, if we arrive to the limit, we
   stop the process and return [`Stack].

   That means, if the client receives [`Stack], he can consider than the file is
   incomplete and he needs to deal with this partial writing in other way than
   the [writer] - abort and use an other way to write what he wants.

   * If the [writer] returns an error, we stop the process and return [`Writer
   error].
   * If the [encoder] returns an error, we stop the process and return [`Encoder
   error].

   In all case, we don't close the file-descriptor.

   Otherwise, we return [Ok result] with the result of the encoder. *)

let safe_encoder_to_file
    (type state) (type raw) (type res) (type err_encoder) (type err_writer)
    ~limit
    (encoder : (state, raw, res, err_encoder) encoder)
    (writer : ('fd, raw, err_writer) writer)
    (fd : 'fd)
    (raw : raw)
    (state : state) : (res, [ `Stack | `Encoder of err_encoder | `Writer of err_writer ]) result Lwt.t =

  let module E =
    (val encoder : ENCODER with type state = state
                            and type raw = raw
                            and type result = res
                            and type error = err_encoder)
  in

  let open Lwt.Infix in

  let rec go ~stack ?(rest = 0) state =
    if stack < limit
    then E.eval raw state >>= function
      | `Error (state, err) -> Lwt.return (Error (`Encoder err))
      | `End (state, res) ->
        if E.used state + rest > 0
        then writer raw ~off:0 ~len:(rest + E.used state) fd >>= function
          | Ok n ->
            if n = E.used state + rest
            then Lwt.return (Ok res)
            else go ~stack:(stack + 1) (E.flush n ((E.used state + rest) - n) state)
          | Error err -> Lwt.return (Error (`Writer err))
        else Lwt.return (Ok res)
      | `Flush state ->
        writer raw ~off:0 ~len:(rest + E.used state) fd >>= function
        | Ok n ->
          if n = rest + E.used state
          then go ~stack:0 (E.flush 0 (E.raw_length raw) state)
          else begin
            let rest = (rest + E.used state) - n in
            E.raw_blit raw n raw 0 rest;
            go
              ~stack:(if E.raw_length raw - rest = 0 then stack + 1 else 0)
              ~rest
              (E.flush rest (E.raw_length raw - rest) state)
          end
        | Error err -> Lwt.return (Error (`Writer err))
    else Lwt.return (Error `Stack)
  in

  go ~stack:0 state
