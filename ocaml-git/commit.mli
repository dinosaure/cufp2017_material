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

module type S =
sig
  module Hash
    : Ihash.S
  (** The [Hash] module used to make this interface. *)

  type t
  (** A Git Commit object. Which specifies the top-level {!Tree.t} for the
      snapshot of the project at a point; the author/{i committer} information and
      the commit message. *)

  module D
    : S.DECODER  with type t = t
                  and type raw = Cstruct.t
                  and type init = Cstruct.t
                  and type error = [ `Decoder of string ]
  (** The decoder of the Git Commit object. We constraint the input to be a
      {Cstruct.t}. This decoder needs a {Cstruct.t} as an internal buffer. *)

  module A
    : S.ANGSTROM with type t = t
  (** The Angstrom decoder of the Git Commit object. *)

  module F
    : S.FARADAY  with type t = t
  (** The Faraday encoder of the Git Commit object. *)

  module M
    : S.MINIENC  with type t = t
  (** The {!Minienc} encoder of the Git Commit object. *)

  module E
    : S.ENCODER  with type t = t
                  and type raw = Cstruct.t
                  and type init = int * t
                  and type error = [ `Never ]
  (** The encoder (which uses a {!Minienc.encoder}) of the Git Commit object. We
      constraint the output to be a {Cstruct.t}. This encoder needs the Commit
      OCaml value and the memory consumption of the encoder (in bytes). The
      encoder can not fail.

      NOTE: we can not unspecified the error type (it needs to be concrete) but,
      because the encoder can not fail, we define the error as [`Never]. *)

  include Ihash.DIGEST with type t := t
                        and type hash = Hash.t

  include S.BASE with type t := t

  val parents : t -> Hash.t list
  (** [parents c] returns all parents of the Git Commit object [c]. *)

  val tree : t -> Hash.t
  (** [tree c] returns the hash of top-level {!Tree.t} of the Git Commit object
      [c]. *)

  val compare_by_date : t -> t -> int
  (** [compare_by_date a b] compares the Git Commit object [a] and [b] by the
      date of the author. *)
end

module Make
    (H : Ihash.S with type Digest.buffer = Cstruct.t
                  and type hex = string)
  : S with module Hash = H
(** The {i functor} to make the OCaml representation of the Git Commit object by
    a specific hash implementation. We constraint the {!Ihash.S} module to
    compute a {Cstruct.t} flow and generate a [string] as the hexadecimal
    representation of the hash. *)
