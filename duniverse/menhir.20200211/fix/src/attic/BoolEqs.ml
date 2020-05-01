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

(* Construction. *)

let disjunction f xs =
  List.fold_left (fun formula x -> FDisjunction (formula, f x)) FFalse xs

let conjunction f xs =
  List.fold_left (fun formula x -> FConjunction (formula, f x)) FTrue xs

(* Evaluation. *)

let rec eval f env =
  match f with
  | FVar x ->
      env x
  | FTrue ->
      true
  | FFalse ->
      false
  | FDisjunction (f1, f2) ->
      eval f1 env || eval f2 env
  | FConjunction (f1, f2) ->
      eval f1 env && eval f2 env

(* -------------------------------------------------------------------------- *)

(* We are now ready to solve. *)

module Make
  (M : IMPERATIVE_MAPS)
= struct

  type variable = M.key
  type valuation = variable -> bool
  type equations = variable -> variable formula

  module F = Fix.Make(M)(Boolean)

  let lfp (eqs : equations) : valuation =
    let eqs : F.equations =
      fun v -> eval (eqs v)
    in
    F.lfp eqs

end

