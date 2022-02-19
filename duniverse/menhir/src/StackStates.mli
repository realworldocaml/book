(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module performs a static analysis of the LR(1) automaton in order to
   determine which states might possibly be held in the known suffix of the
   stack at every state. *)

open Grammar

(**The functors [Run] and [Dummy] assume that the height of the known suffix
   of the stack is known at every state. This height information must be
   consistent: the height at a state [s] must be no greater than the minimum
   of the height at the predecessors of [s], plus one. *)
module type STACK_HEIGHTS = sig

  (**[stack_height s] is the height of the known suffix of the stack
     at state [s]. *)
  val stack_height: Lr1.node -> int

  (**[production_height prod] is the height of the known suffix of the stack
     at a state where production [prod] can be reduced. *)
  val production_height: Production.index -> int

  (**[goto_height nt] is the height of the known suffix of the stack at a
     state where an edge labeled [nt] has just been followed. *)
  val goto_height: Nonterminal.t -> int

  (**The string [variant] should be "short" or "long" and is printed
     when --timings is enabled. *)
  val variant: string

end

(**This signature describes the output of the functors [Run] and [Dummy]. *)
module type STACK_STATES = sig

  (**A property is a description of the known suffix of the stack at state
     [s]. It is represented as an array. By convention, the top of the stack
     is the end of the array. Each array element is a set of states that may
     appear in this stack cell. *)
  type property =
    Lr1.NodeSet.t array

  (**[stack_states s] is the known suffix of the stack at state [s]. *)
  val stack_states: Lr1.node -> property

  (**[production_states prod] is the known suffix of the stack at a state
     where production [prod] can be reduced. In the short invariant, the
     length of this suffix is [Production.length prod]. In the long
     invariant, its length can be greater. *)
  val production_states: Production.index -> property

  (**[goto_states nt] is the known suffix of the stack at a state where an
     edge labeled [nt] has just been followed. If [long] is false, then the
     length of this suffix is [1]. If [long] is true, then its length can be
     greater. *)
  val goto_states: Nonterminal.t -> property

  (* At log level [-lc 3], the result of the analysis is logged, in an
     unspecified format. *)

end

(**The functor [Run] performs a simple data flow analysis to determine
   which states may appear in each stack cell. *)
module Run (S : STACK_HEIGHTS) : STACK_STATES

(**The dummy module [Dummy] is used to skip the above computation. *)
module Dummy (S : STACK_HEIGHTS) : STACK_STATES
