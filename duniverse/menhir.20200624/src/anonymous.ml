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

open Syntax

(* For each anonymous rule, we define a fresh nonterminal symbol, and
   replace the anonymous rule with a reference to this symbol. If the
   anonymous rule appears inside a parameterized rule, then we must
   define a parameterized nonterminal symbol. *)

(* ------------------------------------------------------------------------ *)

(* Computing the free names of some syntactic categories. *)

let rec fn_parameter accu (p : parameter) =
  (* [p] cannot be [ParameterAnonymous _]. *)
  let x, ps = Parameters.unapp p in
  let accu = StringSet.add (Positions.value x) accu in
  fn_parameters accu ps

and fn_parameters accu ps =
  List.fold_left fn_parameter accu ps

let fn_producer accu ((_, p, _) : producer) =
  fn_parameter accu p

let fn_branch accu branch =
  List.fold_left fn_producer accu branch.pr_producers

let fn_branches accu branches =
  List.fold_left fn_branch accu branches

(* ------------------------------------------------------------------------ *)

(* This functor makes it easy to share mutable internal state between
   the functions that follow. *)

module Run (X : sig end) = struct

(* ------------------------------------------------------------------------ *)

(* A fresh name generator. *)

let fresh : unit -> string =
  let next = ref 0 in
  fun () ->
    Printf.sprintf "__anonymous_%d" (Misc.postincrement next)

(* ------------------------------------------------------------------------ *)

(* A rule accumulator. Used to collect the fresh definitions that we
   produce. *)

let rules =
  ref []

(* ------------------------------------------------------------------------ *)

(* [anonymous pos parameters branches] deals with an anonymous rule,
   at position [pos], which appears inside a possibly-parameterized
   rule whose parameters are [parameters], and whose body is
   [branches]. We assume that [branches] does not itself contain any
   anonymous rules. As a side effect, we create a fresh definition,
   and return its name. *)

let var (symbol : symbol) : parameter =
  ParameterVar (Positions.unknown_pos symbol)

let anonymous pos (parameters : symbol list) (branches : parameterized_branch list) : parameter =
  (* Compute the free symbols of [branches]. They should form a subset
     of [parameters], although we have not yet checked this. We create
     a definition that is parameterized only over the parameters that
     actually occur free in the definition -- i.e., a definition without
     useless parameters. This seems important, as (in some situations)
     it avoids duplication and leads to fewer states in the automaton. *)
  let used = fn_branches StringSet.empty branches in
  let parameters = List.filter (fun x -> StringSet.mem x used) parameters in
  (* Generate a fresh non-terminal symbol. *)
  let symbol = fresh() in
  (* Construct its definition. Note that it is implicitly marked %inline.
     Also, it does not carry any attributes; this is consistent
     with the fact that %inline symbols cannot carry attributes. *)
  let rule = {
    pr_public_flag = false;
    pr_inline_flag = true;
    pr_nt          = symbol;
    pr_positions   = [ pos ]; (* this list is not allowed to be empty *)
    pr_attributes  = [];
    pr_parameters  = parameters;
    pr_branches    = branches
  } in
  (* Record this definition. *)
  rules := rule :: !rules;
  (* Return the symbol that stands for it. *)
  Parameters.app (Positions.with_pos pos symbol) (List.map var parameters)

(* ------------------------------------------------------------------------ *)

(* Traversal code. *)

let rec transform_parameter (parameters : symbol list) (p : parameter) : parameter =
  match p with
  | ParameterVar _ ->
      p
  | ParameterApp (x, ps) ->
      ParameterApp (x, List.map (transform_parameter parameters) ps)
  | ParameterAnonymous branches ->
      let pos = Positions.position branches
      and branches = Positions.value branches in
      (* Do not forget the recursive invocation! *)
      let branches = List.map (transform_parameterized_branch parameters) branches in
      (* This is where the real work is done. *)
      anonymous pos parameters branches

and transform_producer parameters ((x, p, attrs) : producer) =
  x, transform_parameter parameters p, attrs

and transform_parameterized_branch parameters branch =
  let pr_producers =
    List.map (transform_producer parameters) branch.pr_producers
  in
  { branch with pr_producers }

let transform_parameterized_rule rule =
  let pr_branches =
    List.map (transform_parameterized_branch rule.pr_parameters) rule.pr_branches
  in
  { rule with pr_branches }

end

(* ------------------------------------------------------------------------ *)

(* The main entry point invokes the functor and reads its result. *)

let transform_partial_grammar g =
  let module R = Run(struct end) in
  let pg_rules = List.map R.transform_parameterized_rule g.pg_rules in
  let pg_rules = !R.rules @ pg_rules in
  { g with pg_rules }
