(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This is the core of the reachability analysis. After the automaton has been
   constructed, this (expensive) analysis determines exactly under which
   conditions each nonterminal edge in the automaton can be taken. This
   information can then be used to determine how to reach certain states
   in the automaton; see, e.g., [LRijkstra].

   [LRijsktraFast] implements a faster alternative.  *)

(* In this analysis, we explicitly ignore the [error] token. (We display a
   warning if the grammar uses this token.) Thus, we disregard any reductions
   or transitions that take place when the lookahead symbol is [error]. As a
   result, any state whose incoming symbol is [error] is found unreachable. It
   would be too complicated to have to create a first error in order to be
   able to take certain transitions or drop certain parts of the input. *)

module Run
    (X : sig
       (* If [verbose] is set, produce various messages on [stderr]. *)
       val verbose: bool
     end)
    () :
sig
  open Grammar

  include LRijkstra.REACHABILITY_RESULT

  (* [query s nt a] enumerates all words [w] and all symbols [z] such that, in
     state [s], the outgoing edge labeled [nt] can be taken by consuming the
     word [w], under the assumption that the next symbol is [z], and the first
     symbol of the word [w.z] is [a]. *)
  val query:
    (* s:  *) Lr1.node ->
    (* nt: *) Nonterminal.t ->
    (* a:  *) Terminal.t ->
    (* f:  *) (Word.t -> Terminal.t -> unit) ->
              unit

end
