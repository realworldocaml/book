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

module Solve
  (S : SYNTAX)
= struct

open S

(* -------------------------------------------------------------------------- *)

(* A system of arbitrary equations is turned into a system of small
   equations by chopping formulae so as to ensure that their size is
   bounded by a constant [k]. *)

let k = 32 (* TEMPORARY parameterize? *)

(* In the new equation system, variables (known as ``points'') are
   either original variables, or roots of small formulae whose leaves
   are again points. Each such root carries a mutable field for use by
   the [Fix] module. This allows [Fix] to perform memoization at each
   formula root, and guarantees linear time complexity. *)

type ('variable, 'property) dummy = (* TEMPORARY *)
    ('variable, 'property) info

type ('variable, 'property) point =
    | V of 'variable
    | R of ('variable, 'property) root

and ('variable, 'property) root = {
  formula: ('variable, 'property) point formula; (* a small formula *)
  mutable data: ('variable, 'property) info option;
}

and ('variable, 'property) info =
    (('variable, 'property) point, 'property) dummy

let mkV v =
  V v

(* -------------------------------------------------------------------------- *)

module Make
  (V : sig type variable end)
  (P : PROPERTY with type property = S.property)
  (M : IMPERATIVE_MAP with type key = V.variable
		       and type data = (V.variable, P.property) info)
  (E : SYNTACTIC_EQUATIONS with type variable = V.variable
			    and type 'variable formula = 'variable S.formula)
= struct

(* -------------------------------------------------------------------------- *)

(* Conversion of arbitrary formulae to small formulae. *)

module SmallEquations = struct

  type variable =
      (E.variable, P.property) point

  type property =
      P.property

  let rec chop (f : E.variable formula) : variable formula =
    
    let credit = ref k in
    
    let rec chopc (f : E.variable formula) : variable formula =
      let cost = cost f in
      if !credit <= 0 && cost > 0 then
	var (R { formula = chop f; data = None })
      else (
	credit := !credit - cost;
	map mkV chopc f
      )

    in
    chopc f

  (* Because [rhs] is invoked at most once per variable, each right-hand side
     is chopped at most once. Thus, at most one copy of each formula root exists.
     This allows storing information about the root within the root itself. *)

  let rhs : variable -> variable formula = function
    | V v ->
	chop (E.rhs v)
    | R root ->
	root.formula

  let rhs (v : variable) : (variable -> property) -> property =
    eval (rhs v)

end

(* -------------------------------------------------------------------------- *)

(* An imperative map over points is implemented in terms of the existing
   imperative map over variables, on the one hand, and of the mutable
   storage field found within each formula root, on the other hand. *)

module MixedMap = struct

  type key =
      SmallEquations.variable

  type data =
      (V.variable, P.property) info

  let set (key : key) (data : data) : unit =
    match key with
    | V x ->
	M.set x data
    | R root ->
	root.data <- Some data

  let get (key : key) : data option =
    match key with
    | V x ->
	M.get x
    | R root ->
	root.data

end

(* -------------------------------------------------------------------------- *)

(* We are now ready to solve the small equation system. *)

module Solution =
  Make (SmallEquations) (P) (MixedMap) (SmallEquations)

(* -------------------------------------------------------------------------- *)

(* Publish the solution (at original variables only). *)

type variable =
    E.variable

type property =
    P.property

let get (v : variable) =
  Solution.get (V v)

end
end

(* TEMPORARY measure performance without (singular) imperative map; use
   (plural) imperative maps instead, implemented as a hash table or an
   infinite array; see if it's worth the trouble of publishing those info
   types. *)

