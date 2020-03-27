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

(* This module constructs an LALR automaton for the grammar described by the
   module [Grammar]. *)

(* In LALR mode, two LR(1) states are merged as soon as they have the same
   LR(0) core. *)

open Grammar

type lr1state =
  Lr0.lr1state

module Run () = struct
let () = ()

(* -------------------------------------------------------------------------- *)

(* The LALR automaton has exactly the same states as the LR(0) automaton, up
   to lookahead information. Therefore, we can use the same state numbers.
   Thus, the states and the transitions of the LALR automaton are the same as
   those of the LR(0) automaton! *)

(* This means that we have almost nothing to do: in fact, the only thing that
   we have to do is compute a mapping of LR(0) nodes to LR(1) states. *)

(* This computation can be viewed as a fixed point computation. In fact, it is
   a special kind of fixed point computation: it can be viewed as a forward
   data flow analysis where the graph is the LR(0) automaton and a property is
   an LR(1) state. *)

type node =
  int

(* A property is an LR(1) state. The function [join] is used to merge the
   contributions of multiple predecessor states. The function [leq] is used to
   detect stabilization. *)

module P = struct
  type property = lr1state
  let leq = Lr0.subsume
  let join = Lr0.union
end

(* The graph. *)

module G = struct

  type variable = node
  type property = P.property

  (* The root nodes are the entry nodes of the LR(0) automaton. The properties
     associated with these nodes are given by the function [Lr0.start]. *)

  let foreach_root f =
    ProductionMap.iter (fun _prod node ->
      f node (Lr0.start node)
    ) Lr0.entry

  (* The edges are the edges of the LR(0) automaton, and the manner in which
     each edge contributes to the computation of a property is given by the
     function [Lr0.transition]. *)

  let foreach_successor node state f =
    SymbolMap.iter (fun symbol (successor_node : node) ->
      let successor_state : lr1state = Lr0.transition symbol state in
      f successor_node successor_state
    ) (Lr0.outgoing_edges node)

end

(* Run the data flow computation. *)

module F = Fix.DataFlow.ForIntSegment(Lr0)(P)(G)
  (* [solution : variable -> property option]. *)
  (* Because every node is reachable, this function never returns [None]. *)

(* -------------------------------------------------------------------------- *)

(* Expose the mapping of nodes to LR(1) states. *)

let n =
  Lr0.n

let states : lr1state array =
  Array.init n (fun node -> Option.force (F.solution node))

let state : node -> lr1state =
  Array.get states

(* -------------------------------------------------------------------------- *)

(* Expose the entry nodes and transitions of the LALR automaton. *)

(* Because we re-use LR(0) node numbers, these are exactly the same as those
   of the LR(0) automaton! *)

let entry : node ProductionMap.t =
  Lr0.entry

let transitions : node -> node SymbolMap.t =
  Lr0.outgoing_edges

(* -------------------------------------------------------------------------- *)

(* Expose the bijection between nodes and numbers. *)

let number (i : node) : int =
  i

let node (i : int) : node =
  i

(* -------------------------------------------------------------------------- *)

end (* Run *)
