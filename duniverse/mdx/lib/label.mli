(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Relation : sig
  type t = Eq | Neq | Le | Lt | Ge | Gt

  val pp : Format.formatter -> t -> unit

  val compare : t -> int -> int -> bool

  val raw_parse : string -> string * (t * string) option
  (** [raw_parse s] splits [s] into a label, and optionally the relation and
      the associated value. *)
end

type non_det = Nd_output | Nd_command

val default_non_det : non_det

type t =
  | Dir of string
  | Source_tree of string
  | File of string
  | Part of string
  | Env of string
  | Skip
  | Non_det of non_det option
  | Version of Relation.t * Ocaml_version.t
  | Require_package of string
  | Set of string * string
  | Unset of string

val pp : Format.formatter -> t -> unit

val interpret :
  string ->
  (Relation.t * string) option ->
  (t, [> `Msg of string ]) Result.result

val of_string : string -> (t list, [ `Msg of string ] list) Result.result
(** [of_string s] cuts [s] into a list of labels. *)
