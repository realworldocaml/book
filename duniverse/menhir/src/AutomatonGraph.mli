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

(* Build and print the LR(1) automaton as a graph. Each state of the automaton
   gives rise to a node. Edges are labeled with nonterminal and terminal
   symbols. The reduction actions that exist in each state are not shown. *)

val print_automaton_graph: unit -> unit
