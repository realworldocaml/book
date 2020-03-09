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

(** Test outputs. *)

type t = [ `Output of string | `Ellipsis ]
(** The type for test outputs. Ellipisis ([...]) allow to skip zero,
   one or many lines while comparing the outputs with {!equal}. *)

val equal : t list -> t list -> bool
(** [equal x y] is true iff [x] and [y] are equivalent, modulo
   ellipsis. *)

val merge : [ `Output of string ] list -> t list -> t list
(** [merge output test] merges any [`Ellipsis] items from [test] into
   [output]. *)

val pp : ?pad:int -> t Fmt.t
(** [pp] is the pretty-printer for test outputs. [pad] is the size of
   the optional whitespace left-padding (by default it is 0). *)

val dump : t Fmt.t
(** [dump] is the printer for dumping test outputs. Useful for debugging. *)
