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

(** serialisers to and from {!Ipaddr} and s-expression {!Sexplib0} format 
  
  To use these with ppx-based derivers, simply replace the reference to the
  {!Ipaddr} type definition with {!Ipaddr_sexp} instead. That will import the
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

type t = Ipaddr.t

val sexp_of_t : Ipaddr.t -> Sexplib0.Sexp.t

val t_of_sexp : Sexplib0.Sexp.t -> Ipaddr.t

val compare : Ipaddr.t -> Ipaddr.t -> int

type scope = Ipaddr.scope

val sexp_of_scope : Ipaddr.scope -> Sexplib0.Sexp.t

val scope_of_sexp : Sexplib0.Sexp.t -> Ipaddr.scope

module V4 : sig
  type t = Ipaddr.V4.t

  val sexp_of_t : Ipaddr.V4.t -> Sexplib0.Sexp.t

  val t_of_sexp : Sexplib0.Sexp.t -> Ipaddr.V4.t

  val compare : Ipaddr.V4.t -> Ipaddr.V4.t -> int

  module Prefix : sig
    type addr = Ipaddr.V4.Prefix.addr

    type t = Ipaddr.V4.Prefix.t

    val sexp_of_t : Ipaddr.V4.Prefix.t -> Sexplib0.Sexp.t

    val t_of_sexp : Sexplib0.Sexp.t -> Ipaddr.V4.Prefix.t

    val compare : Ipaddr.V4.Prefix.t -> Ipaddr.V4.Prefix.t -> int
  end
end

module V6 : sig
  type t = Ipaddr.V6.t

  val sexp_of_t : Ipaddr.V6.t -> Sexplib0.Sexp.t

  val t_of_sexp : Sexplib0.Sexp.t -> Ipaddr.V6.t

  val compare : Ipaddr.V6.t -> Ipaddr.V6.t -> int

  module Prefix : sig
    type addr = Ipaddr.V6.Prefix.addr

    type t = Ipaddr.V6.Prefix.t

    val sexp_of_t : Ipaddr.V6.Prefix.t -> Sexplib0.Sexp.t

    val t_of_sexp : Sexplib0.Sexp.t -> Ipaddr.V6.Prefix.t

    val compare : Ipaddr.V6.Prefix.t -> Ipaddr.V6.Prefix.t -> int
  end
end

module Prefix : sig
  type addr = Ipaddr.Prefix.addr

  type t = Ipaddr.Prefix.t

  val sexp_of_t : Ipaddr.Prefix.t -> Sexplib0.Sexp.t

  val t_of_sexp : Sexplib0.Sexp.t -> Ipaddr.Prefix.t

  val compare : Ipaddr.Prefix.t -> Ipaddr.Prefix.t -> int
end
