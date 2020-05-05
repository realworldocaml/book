(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* This is the lattice of the natural numbers, completed with [Infinity], and
   ordered towards zero (i.e. [Infinity] is [bottom], [Finite 0] is [top]). *)

(* These numbers are further enriched with sequences of matching length. Thus,
   a lattice element is either [Finite (n, xs)], where [n] is a natural number
   and [xs] is a sequence of length [n]; or [Infinity]. The sequences [xs] are
   ignored by the ordering (e.g., [compare] ignores them) but are nevertheless
   constructed (e.g., [add] concatenates two sequences). They should be thought
   of as witnesses, or proofs, that explain why the number [n] was obtained. *)

type 'a t =
| Finite of int * 'a Seq.seq
| Infinity

val bottom: 'a t
val equal: 'a t -> 'b t -> bool
val is_maximal: 'a t -> bool

val epsilon: 'a t
val singleton: 'a -> 'a t

val min: 'a t -> 'a t -> 'a t
val add: 'a t -> 'a t -> 'a t

val min_lazy: 'a t -> (unit -> 'a t) -> 'a t
val add_lazy: 'a t -> (unit -> 'a t) -> 'a t

val print: ('a -> string) -> 'a t -> string
val to_int: 'a t -> int
val extract: 'a t -> 'a list
