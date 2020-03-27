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

(* The natural numbers, completed with [Infinity], and ordered towards
   zero (i.e. [Infinity] is [bottom], [Finite 0] is [top]). *)

type t =
| Finite of int
| Infinity

include Fix.PROPERTY with type property = t

val epsilon: t
val singleton: 'a -> t

val min: t -> t -> t
val add: t -> t -> t

val min_lazy: t -> (unit -> t) -> t
val add_lazy: t -> (unit -> t) -> t

val print: t -> string
