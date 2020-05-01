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

(* This module provides a simple-minded implementation of first-order
   unification over an arbitrary signature. *)

(* -------------------------------------------------------------------------- *)

(* The signature must be described by the client, as follows. *)

module type STRUCTURE = sig

  (* The type ['a structure] should be understood as a type of shallow terms
     whose leaves have type ['a]. *)
  type 'a structure

  val map: ('a -> 'b) -> 'a structure -> 'b structure

  val iter: ('a -> unit) -> 'a structure -> unit

  (* [iter2] fails if the head constructors differ. *)
  exception Iter2
  val iter2: ('a -> 'b -> unit) -> 'a structure -> 'b structure -> unit

end

(* -------------------------------------------------------------------------- *)

(* The unifier. *)

module Make (S : STRUCTURE) : sig

  (* The type of unification variables. *)

  type variable

  (* [fresh s] creates a fresh variable that carries the structure [s]. *)

  val fresh: variable S.structure option -> variable

  (* [structure x] returns the structure (currently) carried by variable [x]. *)

  val structure: variable -> variable S.structure option

  (* [unify x y] attempts to unify the terms represented by the variables
     [x] and [y]. The creation of cycles is not permitted; an eager occurs
     check rules them out. *)

  exception Unify of variable * variable
  exception Occurs of variable * variable
  val unify: variable -> variable -> unit

  (* This is the type of deep terms over the signature [S]. *)

  type term =
    | TVar of int (* the variable's unique identity *)
    | TNode of term S.structure

  (* [decode x] turns the variable [x] into the term that it represents.
     Sharing is lost, so this operation can in the worst case have
     exponential cost. *)

  val decode: variable -> term

end
