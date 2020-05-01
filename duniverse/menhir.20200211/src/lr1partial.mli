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

(* This exception is raised by [Run] if we fail to reach the goal state.
   This is known to happen in a few pathological cases (e.g., when a
   shift/reduce conflict is solved in favor of reduction, the only path
   towards the goal state may disappear). So we report this situation
   gracefully in the .conflicts file instead of failing abruptly. *)

exception Oops

module Run (X : sig

  (* A restricted set of tokens of interest. *)

  val tokens: TerminalSet.t

  (* A state of the (merged) LR(1) automaton that we're trying to
     simulate. *)

  val goal: Lr1.node

end) : sig

  (* What we are after is a path, in the canonical LR(1) automaton,
     that leads from some entry node to a node [N] such that (i)
     [N] has a conflict involving one of the tokens of interest
     and (ii) [N] corresponds to the goal node, that is, the path
     that leads to [N] in the canonical LR(1) automaton leads to
     the goal node in the merged LR(1) automaton. *)

  val source: Item.t

  val path: Symbol.t array

  val goal: Lr0.concretelr1state

  (* An (arbitrarily chosen) conflict token in the goal state. *)

  val token: Terminal.t

end

