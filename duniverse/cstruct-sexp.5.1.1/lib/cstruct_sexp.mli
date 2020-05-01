(*
 * Copyright (c) 2012-2019 Anil Madhavapeddy <anil@recoil.org>
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

(** Sexpression serialisers for {!Cstruct.t} values *)

type buffer = Cstruct.buffer
(** [buffer] is an alias for the corresponding {!Cstruct.buffer} type *)

val sexp_of_buffer : Cstruct.buffer -> Sexplib.Sexp.t
(** [sexp_of_buffer b] returns the s-expression representation of the raw memory buffer [b] *)

val buffer_of_sexp : Sexplib.Sexp.t -> Cstruct.buffer
(** [buffer_of_sexp s] returns a fresh memory buffer from the s-expression [s].
    [s] should have been constructed using {!sexp_of_buffer}. *)

type t = Cstruct.t
(** [t] is an alias for the corresponding {!Cstruct.t} type *)

val sexp_of_t : t -> Sexplib.Sexp.t
(** [sexp_of_t t] returns the s-expression representation of the Cstruct [t] *)

val t_of_sexp : Sexplib.Sexp.t -> t
(** [t_of_sexp s] returns a fresh {!Cstruct.t} that represents the
     s-expression previously serialised by {!sexp_of_t}. *)

