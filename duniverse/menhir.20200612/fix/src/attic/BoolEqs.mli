(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

open Fix

(* -------------------------------------------------------------------------- *)

(* Positive Boolean formulae. *)

type 'variable formula =
  | FVar of 'variable
  | FTrue
  | FFalse
  | FDisjunction of 'variable formula * 'variable formula
  | FConjunction of 'variable formula * 'variable formula

(* TEMPORARY supprimer ces deux constructeurs? *)
val disjunction: ('a -> 'variable formula) -> 'a list -> 'variable formula
val conjunction: ('a -> 'variable formula) -> 'a list -> 'variable formula

(* -------------------------------------------------------------------------- *)

(* Solving systems of recursive positive Boolean equations. *)

module Make
  (M : IMPERATIVE_MAPS)
  : sig
    type variable = M.key
    type valuation = variable -> bool
    type equations = variable -> variable formula
    val lfp: equations -> valuation
  end
