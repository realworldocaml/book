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

(* This is an enriched version of [Boolean], where we compute not just a
   Boolean property, [Unreachable] or [Reachable], but also (in the latter
   case) a path (a sequence). *)

(* A property is either [Reachable xs], where [xs] is a path; or [Unreachable]. *)

type 'a t =
| Reachable of 'a Seq.seq
| Unreachable

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
