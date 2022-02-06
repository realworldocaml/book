(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open BasicSyntax

let keep_branch branch : bool =
  let n = List.length branch.producers in
  let warned = ref false in
  branch.producers |> List.iteri begin fun i producer ->
    if i < n - 1 && producer.producer_symbol = "error" && not !warned then begin
      warned := true;
      Error.grammar_warning [branch.branch_position]
        "when --strategy simplified is selected,\n\
         the error token may appear only at the end of a production\n\
         (and the semantic action must abort the parser).\n\
         This production will be ignored."
    end
  end;
  not !warned

let filter_rule rule =
  { rule with branches = List.filter keep_branch rule.branches }

let filter_grammar grammar =
  { grammar with rules = StringMap.map filter_rule grammar.rules }

let filter_grammar grammar =
  match Settings.strategy with
  | `Simplified ->
      filter_grammar grammar
  | `Legacy ->
      grammar

(* -------------------------------------------------------------------------- *)

(* Rejecting the $syntaxerror keyword. *)

(* We could reject this keyword on the fly in the lexer, but the lexer is so
   ugly that it seems preferable to write a separate piece of code here. *)

let check_branch branch =
  if Action.has_syntaxerror branch.action then
    Error.error [branch.branch_position]
      "when --strategy simplified is selected,\n\
       the use of $syntaxerror is forbidden."

let check_rule _nt rule =
  List.iter check_branch rule.branches

let check_grammar grammar =
  StringMap.iter check_rule grammar.rules

let check_grammar grammar =
  match Settings.strategy with
  | `Simplified ->
      check_grammar grammar
  | `Legacy ->
      ()
