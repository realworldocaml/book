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

open BasicSyntax

let rec visit grammar visited symbol =
  try
    let rule = StringMap.find symbol grammar.rules in
    if not (StringSet.mem symbol visited) then
      let visited = StringSet.add symbol visited in
      List.fold_left (visitb grammar) visited rule.branches
    else
      visited
  with Not_found ->
    (* This is a terminal symbol. *)
    assert (symbol = "error" || StringMap.mem symbol grammar.tokens);
    visited

and visitb grammar visited { producers = symbols } =
  List.fold_left (visits grammar) visited symbols

and visits grammar visited producer =
  visit grammar visited (producer_symbol producer)

let trim grammar =
  if StringSet.cardinal grammar.start_symbols = 0 then
    Error.error [] "no start symbol has been declared."
  else
    let reachable =
      StringSet.fold (fun symbol visited ->
        visit grammar visited symbol
      ) grammar.start_symbols StringSet.empty
    in
    StringMap.iter (fun symbol rule ->
      if not (StringSet.mem symbol reachable) then
        Error.grammar_warning
          rule.positions
             "symbol %s is unreachable from any of the start symbol(s)."
               symbol
    ) grammar.rules;
    { grammar with
      rules = StringMap.restrict reachable grammar.rules;
      types = StringMap.restrict reachable grammar.types;
      on_error_reduce = StringMap.restrict reachable grammar.on_error_reduce;
    }
