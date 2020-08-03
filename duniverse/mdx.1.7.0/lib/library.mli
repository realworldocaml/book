(*
 * Copyright (c) 2019 Nathan Rebours <nathan.p.rebours@gmail.com>
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

type t = { base_name : string; sub_lib : string option }
(** The type to represent dune libraries as referred to in '#require' statements
    or in dune files.
    I.e. lib.sub_lib is [{base_name = "lib"; sub_lib = Some "sub_lib"}].
    *)

val equal : t -> t -> bool

val compare : t -> t -> int

val pp : t Fmt.t

val from_string : string -> (t, string) Result.result
(** [from_string s] returns the library represented by [s] or an error if [s]
    isn't a valid library. *)

(** Set of libraries *)
module Set : sig
  include Set.S with type elt = t

  val to_package_set : t -> Astring.String.Set.t
  (** [to_package_set lib_set] returns the set of dune packages needed to get
      all the libraries in [lib_set].
      I.e. it's the set of basenames for all the libraries in [lib_set]. *)
end
