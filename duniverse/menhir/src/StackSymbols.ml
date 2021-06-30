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

open Grammar

(* We compute a lower bound on the height of the stack at every state, and at
   the same time, we compute which symbols are held in this stack prefix. *)

(* In order to compute a lower bound on the height of the stack at a state
   [s], we examine the LR(0) items that compose [s]. For each item, if the
   bullet is at position [pos], then we can be assured that the height of the
   stack is at least [pos]. Thus, we compute the maximum of [pos] over all
   items (of which there is at least one). *)

(* The set of items that we use is not closed, but this does not matter; the
   items that would be added by the closure would not add any information
   regarding the height of the stack, since the bullet is at position 0 in
   these items. *)

(* Instead of computing just the stack height, we compute, in the same manner,
   which symbols are on the stack at a state [s]. This is an array of symbols
   whose length is the height of the stack at [s]. By convention, the top of
   the stack is the end of the array. *)

(* This analysis is extremely fast: on an automaton with over 100,000 states,
   it takes under 0.01 second. *)

module Run () = struct

  (* Compute and tabulate this information at the level of the LR(0)
     automaton. *)

  let stack_symbols : Lr0.node -> Symbol.t array =
    let dummy =
      Array.make 0 (Symbol.T Terminal.sharp)
    in
    Misc.tabulate Lr0.n (fun node ->
      Item.Set.fold (fun item accu ->
        let _prod, _nt, rhs, pos, _length = Item.def item in
        if pos > Array.length accu then Array.sub rhs 0 pos else accu
      ) (Lr0.items node) dummy
    )

  (* Extend it to the LR(1) automaton. *)

  let stack_symbols (node : Lr1.node) : Symbol.t array =
    stack_symbols (Lr0.core (Lr1.state node))

  (* Printing. *)

  let buffer =
    Buffer.create 1024

  let print_stack_symbols node =
    stack_symbols node |> Array.iter (fun symbol ->
      Printf.bprintf buffer " %s" (Symbol.print symbol)
    );
    let s = Buffer.contents buffer in
    Buffer.clear buffer;
    s

end
