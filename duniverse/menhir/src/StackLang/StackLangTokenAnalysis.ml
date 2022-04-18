(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module performs a simple forward data flow analysis in order to
   determine (an overapproximation of) the set of possible tokens stored in
   the [token] register at each program point where this register is defined.

   This information is then used to transform the program: dead branches in
   [ICaseToken] constructs are removed, and [ICaseToken] constructs with only
   one live branch are eliminated. *)

(* The command [menhir --list-errors] computes an exact solution to this
   problem: it determines exactly which pairs [(s, z)] of a state [s] and a
   lookahead token [z] are reachable. In contrast, here, we use a simpler
   algorithm and compute only an approximation. *)

(* We could conceivably trust and use [menhir --list-errors] instead of
   performing our own (simpler, coarser) analysis. However, 1- the code in the
   modules LRijkstra* is currently not set up for this kind of use; and 2- the
   StackLang code is not necessarily isomorphic to the LR automaton, because
   of the optimizations (specialization, etc.) that are performed in
   StackLang. Exploiting the static analysis in LRijkstra* would probably
   requiring embedding the analysis data (as [assume] instructions) in the
   StackLang code when it is first produced, so as to allow this information
   to be preserved when the code is transformed. Perhaps this could be done in
   the future. *)

open Printf
open StackLang

(* -------------------------------------------------------------------------- *)

(* The [token] register. *)

let token =
  EmitStackLang.token

(* We compute sets of terminal symbols. *)

module P = struct
  include Grammar.TerminalSet
  let diff xs ys = fold remove ys xs
  type property = t
  let leq_join = union
end

type env =
  P.property

(* -------------------------------------------------------------------------- *)

(* The forward data flow analysis. *)

(* [exec jump env block] analyzes the block [block]. The environment [env],
   a set of terminal symbols, is an overapproximation of the tokens that may
   appear in the [token] register, if this register is defined. The function
   [jump] deals with jumps towards other blocks. *)

let rec exec (jump : label -> env -> unit) env block =
  match block with

  | IPrim (p, PrimLexerCall _, block) ->
      assert (p = PReg token);
      (* The register [token] is overwritten. No knowledge about the (new)
         current token is available. Its value is arbitrary. *)
      let env = P.universe in
      exec jump env block

  | IJump label ->
      jump label env

  | ICaseToken (r, branches, odefault) ->
      assert (r = token);
      List.iter (exec_tokbranch jump env) branches;
      Option.iter (exec_default jump env branches) odefault

  | IPush _
  | IPop _
  | IPeek _
  | IDef _
  | IPrim _
  | ITrace _
  | IComment _
  | IDead _
  | IStop _
  | IReturn _
  | ICaseTag _
    -> Block.iter (exec jump env) block

and exec_tokbranch jump env (tokpat, block) =
  (* Intersect [env] with the tokens that [tokpat] selects. *)
  let env = P.inter env (tokens tokpat) in
  maybe_exec jump env block

and exec_default jump env branches block =
  (* Compute which tokens are implicitly selected by the default branch. These
     are the tokens that are not explicitly selected by a previous branch.
     Intersect [env] with these tokens. *)
  let env = P.diff env (all_tokens branches) in
  maybe_exec jump env block

and maybe_exec jump env block =
  (* If [env] is empty, then this branch is dead and should not be followed.
     Following it would be correct, but could make the data flow analysis
     less precise. *)
  if not (P.is_empty env) then
    exec jump env block

(* The main analysis function. *)

let analyze program : label -> env option =
  let module G = struct
    type variable = label
    type property = env
    let foreach_root jump =
      program.entry |> StringMap.iter begin fun _name label ->
        (* The [token] register is initially undefined, so it does not matter
           what value of [env] we use here. The value of [env] is relevant
           only where [tok] is defined. *)
        jump label P.empty
      end
    let foreach_successor label env jump =
      let block = (lookup program label).block in
      exec jump env block
  end in
  let module D = Fix.DataFlow.ForHashedType(Label)(P)(G) in
  Time.tick "StackLang: static analysis of the current token";
  D.solution

(* -------------------------------------------------------------------------- *)

(* [no_assigns tokpat] returns [true] if the pattern [tokpat] assigns no
   register. *)

let no_assigns tokpat =
  match tokpat with
  | TokSingle (_tok, _r) ->
      (* This pattern assigns the register [r]. *)
      false
  | TokMultiple _toks ->
      (* This pattern assigns no registers. *)
      true

(* [make_exhaustive branches odefault] returns a new optional default branch
   [odefault], so as to guarantee that [branches] and [odefault] together
   form an exhaustive case analysis construct. *)

let make_exhaustive branches odefault =
  if exhaustive branches then begin
    (* This [case] construct was exhaustive already, and still is. *)
    assert (odefault = None);
    odefault
  end
  else match odefault with
  | Some _ ->
      (* This [case] construct has a default branch already. We keep this
         default branch, and the [case] construct remains exhaustive. *)
      odefault
  | None ->
      (* This [case] construct has no default branch, because it is used to
         be exhaustive, but is no longer exhaustive now that some dead
         branches have been removed. Introduce a default branch. *)
      Some (IDead `Dynamic)

(* [comment env block] is invoked when a [case] construct is removed.
   It inserts a comment. *)

let threshold = 5

let comment env block =
  let msg =
    if P.is_singleton env then
      (* This is by far the most common case in practice. *)
      sprintf "The current token is %s." (P.print env)
    else
      let c = P.cardinal env in
      if c <= threshold then
        sprintf "The current token is among %s." (P.print env)
      else
        sprintf
          "The current token is among a set of cardinality %d (not shown)." c
  in
  IComment (msg, block)

(* -------------------------------------------------------------------------- *)

(* The program transformation. *)

exception Break

(* [map env block] transforms the block [block]. The parameter [env] plays
   the same role and is computed in the same way as in [exec jump env block]
   above. *)

let rec map env block : block =
  match block with

  | ICaseToken (r, branches, odefault) ->
      (* This is the interesting case. *)
      map_casetok env r branches odefault

  | IPrim (p, PrimLexerCall vs, block) ->
      let env = P.universe in
      IPrim (p, PrimLexerCall vs, map env block)

  | IPush _
  | IPop _
  | IPeek _
  | IDef _
  | IPrim _
  | ITrace _
  | IComment _
  | IDead _
  | IStop _
  | IReturn _
  | IJump _
  | ICaseTag _
    -> Block.map (map env) block

and map_casetok env r branches odefault =
  (* It should be the case that [env] is nonempty. (Otherwise, this
     code is dead and we should not be transforming it.) As a result,
     at least one branch should be live. *)
  assert (not (P.is_empty env));
  (* Determine whether the default branch is dead. *)
  let default_tokens = P.diff env (all_tokens branches) in
  let dead_default = P.is_empty default_tokens in
  (* Transform every live branch, and drop every dead branch. *)
  let branches = Misc.filter_map (map_tokbranch env) branches in
  (* If the default branch is dead and if there is only one explicit
     branch (whose pattern does not assign any register) then this
     [case] construct is redundant and can be removed. *)
  begin try
    if not (dead_default && List.length branches = 1) then raise Break;
    let branch = Misc.single branches in
    let tokpat, block = branch in
    if not (no_assigns tokpat) then raise Break;
    (* Keep only the live branch. *)
    let env = P.inter env (tokens tokpat) in
    comment env (map env block)
  with Break ->
    (* The [case] construct must be kept. It may be the case that the
       default branch is dead, in which case we remove it. *)
    let odefault = if dead_default then None else odefault in
    (* Transform the default branch (if there is one). *)
    let env = default_tokens in
    let odefault = Option.map (map env) odefault in
    (* Now, if the [case] construct is not exhaustive (because we have removed
       ordinary branches and/or the default branch) then we now add a new
       default branch, so that the [case] construct remains exhaustive. Its
       body is a dynamic [IDead] instruction, so it is clearly recognizable as
       a dead branch. *)
    let odefault = make_exhaustive branches odefault in
    ICaseToken (r, branches, odefault)
  end

and map_tokbranch env (tokpat, block) =
  let env = P.inter env (tokens tokpat) in
  if P.is_empty env then None else Some (tokpat, map env block)

let map_tblock env tblock =
  { tblock with block = map env tblock.block }

(* The main transformation function. *)

let transform program : program =
  let analysis : label -> env option = analyze program in
  let cfg = Label.Map.mapi (fun label tblock ->
    match analysis label with
    | Some env ->
        map_tblock env tblock
    | None ->
        (* This block is apparently dead. No need to transform it. *)
        tblock
  ) program.cfg in
  Time.tick "StackLang: simplifying case analyses on the current token";
  { program with cfg }
