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

(* This module constructs an LR(1) automaton by following Pager's
   method, that is, by merging states on the fly when they are found
   to be (weakly) compatible. *)

(* Shift/reduce conflicts are silently resolved when (and only when)
   that is allowed in a clean way by user-specified priorities. This
   includes shift/reduce/reduce conflicts when (and only when) there
   is agreement that the shift action should be preferred. Conflicts
   that cannot be silently resolved in this phase will be reported,
   explained, and arbitrarily resolved immediately before code
   generation. *)

(* ------------------------------------------------------------------------- *)
(* Accessors. *)

(* This is the type of the automaton's nodes. *)

type node

module Node : Set.OrderedType with type t = node

module NodeSet : Set.S with type elt = node

module NodeMap : Map.S with type key = node

(* These are the automaton's entry states, indexed by the start productions. *)

val entry: node ProductionMap.t

(* [fold_entry] folds over [entry]. For convenience, it gives access not only
   to the start production and start state, but also to the nonterminal
   symbol and to the OCaml type associated with this production. *)

val fold_entry:
  (Production.index -> node -> Nonterminal.t -> Stretch.ocamltype -> 'a -> 'a) ->
  'a -> 'a

(* [entry_of_nt] maps a (user) non-terminal start symbol to the corresponding
   start state. [nt_of_entry] does the reverse. *)

val entry_of_nt: Nonterminal.t -> node
val nt_of_entry: node -> Nonterminal.t

(* Nodes are numbered sequentially from [0] to [n-1]. *)

val n: int
val number: node -> int

(* This provides access to the LR(1) state that a node stands for. *)

val state: node -> Lr0.lr1state

(* This converts a start node into the single item that it contains. *)

val start2item: node -> Item.t

(* This maps a node to its incoming symbol, that is, the symbol
   carried by all of the edges that enter this node. A node has zero
   incoming edges (and, thus, no incoming symbol) if and only if it is
   a start node. *)

val incoming_symbol: node -> Symbol.t option

(* This maps a node to its predecessors. *)

val predecessors: node -> node list

(* A view of the backward (reverse) edges as a graph. *)

module BackwardEdges : sig
  type nonrec node = node
  type label = unit
  val foreach_outgoing_edge: node -> (label -> node -> unit) -> unit
end

(* This provides access to a node's transitions and reductions. *)

val transitions: node -> node SymbolMap.t

val reductions: node -> Production.index list TerminalMap.t
  (* or: node -> Lr0.reductions *)

(* (New as of 2012/01/23.) This tells whether a shift/reduce conflict
   in this node was solved in favor of neither (%nonassoc). This implies
   that one must forbid a default reduction at this node. *)

val forbid_default_reduction: node -> bool

(* [has_beforeend s] tests whether the state [s] can reduce a production
   whose semantic action uses [$endpos($0)]. Note that [$startpos] and
   [$endpos] have been expanded away already, so we need not worry about
   the fact that (in an epsilon production) they expand to [$endpos($0)]. *)

val has_beforeend: node -> bool

(* Computing which terminal symbols a state is willing to act upon.

   This function is currently unused, but could be used as part of an error
   reporting system. *)

val acceptable_tokens: node -> TerminalSet.t

(* Iteration over all nodes. The order in which elements are examined,
   and the order of [map]'s output list, correspond to the numeric
   indices produced by [number] above. *)

val fold: ('a -> node -> 'a) -> 'a -> 'a
val iter: (node -> unit) -> unit
val map: (node -> 'a) -> 'a list

(* Tabulation and sum of a function over nodes. *)

val tabulate: (node -> 'a) -> (node -> 'a)
val sum: (node -> int) -> int

(* Iteration over non-start nodes *)
val foldx: ('a -> node -> 'a) -> 'a -> 'a
val iterx: (node -> unit) -> unit

(* Iteration over all edges that carry a certain symbol. Edges are
   grouped in families, where all edges in a single family have the
   same target node. [targets f accu symbol] invokes [f accu sources
   target] once for every family, where [sources] are the sources of
   the edges in the family and [target] is their common target. *)

val targets: ('a -> node list -> node -> 'a) -> 'a -> Symbol.t -> 'a

(* Iteration over all nodes with conflicts. [conflicts f] invokes [f
   toks node] once for every node [node] with a conflict, where [toks]
   are the tokens involved in the conflicts at that node. *)

val conflicts: (TerminalSet.t -> node -> unit) -> unit

(* ------------------------------------------------------------------------- *)
(* Modifications of the automaton. *)

(* This function performs default conflict resolution.

   First, it resolves standard (shift/reduce and reduce/reduce)
   conflicts (thus ensuring that the automaton is deterministic) by
   removing some reduction actions.

   Second, it resolves end-of-stream conflicts by ensuring that states
   that have a reduce action at the pseudo-token "#" have no other
   action.

   It is called after conflicts have been explained and before code
   generation takes place. The automaton is modified in place. *)

val default_conflict_resolution: unit -> unit

(* This function adds extra reduction actions in the face of an error, if
   requested by the user via [%on_error_reduce]. *)

(* It must be called after conflict resolution has taken place. The
   automaton is modified in place. *)

(* If a state can reduce only one production, whose left-hand symbol has
   been declared [%on_error_reduce], then every error action in this
   state is replaced with a reduction action. This is done even though
   this state may have outgoing shift transitions: thus, we are forcing
   one interpretation of the past, among several possible interpretations. *)

val extra_reductions: unit -> unit

(* ------------------------------------------------------------------------- *)
(* Information about which productions are reduced and where. *)

(* [production_where prod] is the set of all states [s] where production
   [prod] might be reduced. It is an error to call this functios before
   default conflict resolution has taken place. *)

val production_where: Production.index -> NodeSet.t
