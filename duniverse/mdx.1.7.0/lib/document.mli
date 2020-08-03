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

(** {2 Lines} *)

type syntax = Syntax.t = Normal | Cram | Mli

(** The type for the lines of a markdown or cram file. *)
type line = Section of (int * string) | Text of string | Block of Block.t

val pp_line : ?syntax:syntax -> line Fmt.t
(** [pp_line] is the pretty-printer for markdown or cram lines. *)

(** {2 Document} *)

type t = line list
(** The type for mdx documents. *)

val pp : ?syntax:syntax -> t Fmt.t
(** [pp] is the pretty printer for mdx documents. Should be idempotent
   with {!of_string}. *)

val to_string : t -> string
(** [to_string t] converts the document [t] to a string. *)

val envs : t -> Ocaml_env.Set.t
