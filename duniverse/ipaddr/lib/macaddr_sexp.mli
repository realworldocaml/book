(*
 * Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
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

(** serialisers to and from {!Macaddr} and s-expression {!Sexplib0} format

  To use these with ppx-based derivers, simply replace the reference to the
  {!Macaddr} type definition with {!Macaddr_sexp} instead. That will import the
  sexp-conversion functions, and the actual type definitions are simply aliases
  to the corresponding type within {!Ipaddr}.  For example, you might do:

  {[
    type t = {
      ip: Ipaddr_sexp.t;
      mac: Macaddr_sexp.t;
    } [@@deriving sexp]
  ]}

  The actual types of the records will be aliases to the main library types,
  and there will be two new functions available as converters.

  {[
     type t = {
       ip: Ipaddr.t;
       mac: Macaddr.t;
     }
     val sexp_of_t : t -> Sexplib0.t
     val t_of_sexp : Sexplib0.t -> t
  ]}
*)

type t = Macaddr.t

val sexp_of_t : Macaddr.t -> Sexplib0.Sexp.t

val t_of_sexp : Sexplib0.Sexp.t -> Macaddr.t

val compare : Macaddr.t -> Macaddr.t -> int

