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

   The original but slower implementation is provided by [LRijkstraClassic]. *)

(* In this analysis, we explicitly ignore the [error] token. (We display a
   warning if the grammar uses this token.) Thus, we disregard any reductions
   or transitions that take place when the lookahead symbol is [error]. As a
   result, any state whose incoming symbol is [error] is found unreachable. It
   would be too complicated to have to create a first error in order to be
   able to take certain transitions or drop certain parts of the input. *)

module Run
    (X : sig
       (* If [validate] is set, [LRijkstraClassic] is also run and the results
          of both algorithms are compared. *)
       val validate : bool

       (* [verbose] is ignored by [LRijkstraFast], but passed to
          [LRijkstraClassic] in validation mode. *)
       val verbose : bool
     end)
    () :
  LRijkstra.REACHABILITY_RESULT
