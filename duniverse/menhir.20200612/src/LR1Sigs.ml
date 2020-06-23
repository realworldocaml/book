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

(* The output signature of several LR(1) automaton construction algorithms. *)

module type LR1_AUTOMATON = sig

  (* An abstract type of nodes, that is, states in the LR(1) automaton. *)

  type node

  (* The number of nodes. *)

  val n: int

  (* Nodes are numbered from 0 to [n-1]. *)

  val number: node -> int
  val node: int -> node

  (* To each start production corresponds an entry node. *)

  val entry : node ProductionMap.t

  (* Each node carries outgoing transitions towards other nodes. (Note to
     implementors of the signature [LR1_AUTOMATON]: there is no need to
     memoize this function; this is done a posteriori, in [Lr1].) *)

  val transitions: node -> node SymbolMap.t

  (* Each node represents an LR(1) state, that is, a set of LR(1) items. *)

  val state: node -> Lr0.lr1state

end
