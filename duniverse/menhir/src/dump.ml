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

open Printf
open Grammar

module Make (Default : sig

  val has_default_reduction:
    Lr1.node -> (Production.index * TerminalSet.t) option

end) = struct

let dump_node out print_stack_symbols node =

  (* Print the state number. *)

  fprintf out "State %d:\n"
    (Lr1.number node);

  (* Print the known prefix of the stack. *)

  fprintf out
    "## Known stack suffix:\n\
     ##%s\n"
    (print_stack_symbols node);

  (* Print the items. *)

  fprintf out "## LR(1) items:\n%s"
    (Lr0.print "" (Lr1.state node));

  (* Print the transitions. *)

  fprintf out "## Transitions:\n";
  SymbolMap.iter (fun symbol node ->
    fprintf out "-- On %s shift to state %d\n"
      (Symbol.print symbol) (Lr1.number node)
  ) (Lr1.transitions node);

  (* Print the reductions. *)

  begin match Default.has_default_reduction node with
  | Some (prod, toks) ->
      (* There is a default reduction. *)
      (* Because end-of-stream conflicts have been resolved, either [toks]
         is the singleton set that contains just the token [#], or it is
         a set of ordinary terminal symbols. In the former case, Menhir
         reduces without even asking for the next token; in the latter case,
         it first reads the next token, then reduces without looking at it. *)
      assert (
        TerminalSet.equal toks (TerminalSet.singleton Terminal.sharp) ||
        not (TerminalSet.mem Terminal.sharp toks)
      );
      let keyword =
        if TerminalSet.mem Terminal.sharp toks then "Without" else "After"
      in
      fprintf out "## Default reduction:\n";
      fprintf out "-- %s reading the next token, %s\n"
        keyword (Production.describe false prod);

  | None ->
      (* There is no default reduction. *)
      fprintf out "## Reductions:\n";
      (* 2020/11/21: for better readability, we now group the symbols that
         lead to reducing the same production. *)
      let reductions = Lr0.invert (Lr1.reductions node) in
      ProductionMap.iter (fun prod toks ->
        fprintf out "-- On %s\n" (TerminalSet.print toks);
        fprintf out "--   %s\n" (Production.describe false prod)
      ) reductions
  end;

  (* Print the conflicts. *)

  if not (TerminalSet.is_empty (Lr1.conflict_tokens node)) then
    fprintf out "** Conflict on %s\n"
      (TerminalSet.print (Lr1.conflict_tokens node));

  (* Print the end-of-stream conflicts. *)

  Lr1.has_eos_conflict node |> Option.iter begin fun (prods, toks) ->

    (* If this function is invoked before conflict resolution has been
       performed, then the list [prods] could have several elements.
       We pick one. *)
    assert (prods <> []);
    let prod = List.hd prods in

    fprintf out "** End-of-stream conflict on %s\n"
      (TerminalSet.print toks);
    fprintf out
      "**   There is a tension between\n\
       **   (1) %s\n\
       **   without even requesting a lookahead token, and\n\
       **   (2) testing whether the lookahead token is a member of the above set.\n"
      (Production.describe true prod)

  end;

  (* Skip a line. *)

  fprintf out "\n"

let dump filename =
  let module SS = StackSymbols.Run() in
  let out = open_out filename in
  Lr1.iter (dump_node out SS.print_stack_symbols);
  close_out out;
  Time.tick "Dumping the LR(1) automaton"

end
