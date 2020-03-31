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

open Maps
open Fix

(* -------------------------------------------------------------------------- *)

(* Systems of syntactic equations. *)

module type SYNTACTIC_EQUATIONS = sig
  type variable
  type 'variable formula
  val rhs: variable -> variable formula
end

(* -------------------------------------------------------------------------- *)

(* Generic syntax for right-hand sides. *)

module type SYNTAX = sig
  type 'variable formula
  type property
  val var: 'a -> 'a formula
  val cost: 'a formula -> int
  val map: ('a -> 'b) -> ('a formula -> 'b formula) -> 'a formula -> 'b formula
  val eval: 'variable formula -> ('variable -> property) -> property
end

(* -------------------------------------------------------------------------- *)

(* Chopping and solving. *)

module Solve
  (S : SYNTAX)
: sig

  type ('variable, 'property) info

  module Make
    (V : sig type variable end)
    (P : PROPERTY with type property = S.property)
    (M : IMPERATIVE_MAP with type key = V.variable
			 and type data = (V.variable, P.property) info)
    (E : SYNTACTIC_EQUATIONS with type variable = V.variable
			      and type 'variable formula = 'variable S.formula)
    : VALUATION with type variable = V.variable
		and type property = P.property

end

