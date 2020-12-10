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

(* This module implements sort inference. *)

(* -------------------------------------------------------------------------- *)

(* The syntax of sorts is:

     sort ::= (sort, ..., sort) -> *

   where the arity (the number of sorts on the left-hand side of the arrow)
   can be zero. See [GroundSort]. *)

type 'a structure =
  | Arrow of 'a list

type sort =
  | TVar of int
  | TNode of sort structure

(* -------------------------------------------------------------------------- *)

(* Sort unification. *)

type variable

val star: variable
val arrow: variable list -> variable
val fresh: unit -> variable

(* [domain] is the opposite of [arrow]. If [x] has been unified with an arrow,
   then [domain x] returns its domain. Otherwise, it returns [None]. Use with
   caution. *)
val domain: variable -> variable list option

exception Unify of variable * variable
exception Occurs of variable * variable
val unify: variable -> variable -> unit

(* Once unification is over, a unification variable can be decoded as a sort. *)

val decode: variable -> sort

(* Grounding a sort replaces all sort variables with the sort [*]. *)

val ground: sort -> GroundSort.sort
val unground: GroundSort.sort -> sort

(* -------------------------------------------------------------------------- *)

(* A sort can be printed. *)

val print: sort -> string
