(*
 * Copyright (c) 2007-2014 Dave Scott <dave.scott@citrix.com>
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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
 *
 *)

(** Radix tree that can do longest-prefix searches on string keys *)

(** Radix tree that maps [string] keys to ['a] values *)
type 'a t [@@deriving sexp]

(** An empty tree *)
val empty : 'a t

(** [insert key value tree] returns a new tree with the
    mapping [key] to [value] *)
val insert : string -> 'a -> 'a t -> 'a t

(** [longest_prefix key tree] finds the key [k] which shares
    the longest prefix with [key] and returns the associated
    value. *)
val longest_prefix : string -> 'a t -> 'a option

(** [fold f initial t] folds [f] over all bindings in [t] *)
val fold : (string -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b

(** [is_prefix a b] returns true if [a] is a prefix of [b] *)
val is_prefix: string -> string -> bool
