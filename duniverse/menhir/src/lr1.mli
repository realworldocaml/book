(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar

(* This module first constructs an LR(1) automaton by using an appropriate
   construction method (LALR, Pager, canonical).
   Then, this automaton is further transformed (in place), in three steps:

   1. Silent conflict resolution (without warnings),
      following the user's precedence declarations.
      This is done immediately.
      This can remove transitions and reductions.

   2. Default conflict resolution (with warnings),
      following a fixed default policy.
      This is done via an explicit call to [default_conflict_resolution()].
      This can remove reductions.

   3. Addition of extra reductions,
      following the user's [%on_error_reduce] declarations.
      This is done via an explicit call to [extra_reductions()].

   Conflicts are explained after step 1, and before steps 2 and 3.
   This is the main reason why these steps are separate. *)

(* During step 1, shift/reduce conflicts are silently resolved if (and only
   if) that is allowed in a clean way by user-specified priorities. This
   includes multi-way shift/reduce/reduce conflicts if (and only if) there is
   agreement that the shift action should be preferred. Conflicts that cannot
   be silently resolved in this phase are reported, explained, then
   arbitrarily resolved in step 2. *)

(* ------------------------------------------------------------------------- *)
(* Accessors. *)

(* This is the type of the automaton's nodes. *)

type node

module Node : Set.OrderedType with type t = node

module NodeSet : sig
  include Set.S with type elt = node
  val leq_join: t -> t -> t
  val print: t -> string
end

module NodeMap : Map.S with type key = node

module ImperativeNodeMap :
  Fix.MINIMAL_IMPERATIVE_MAPS with type key = node

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
val of_number : int -> node

(* A state is printed simply as its number. *)

val print: node -> string

(* This provides access to the LR(1) state that a node stands for. *)

val state: node -> Lr0.lr1state

(* This converts a start node into the single item that it contains. *)

val start2item: node -> Item.t

(* This maps a node to its incoming symbol, that is, the symbol
   carried by all of the edges that enter this node. A node has zero
   incoming edges (and, thus, no incoming symbol) if and only if it is
   a start node. *)

val incoming_symbol: node -> Symbol.t option

(* [is_start s] determines whether [s] is an initial state. *)

val is_start: node -> bool

(* With each start production [S' -> S], exactly two states are
   associated: a start state, which contains the item [S' -> . S [#]],
   and an exit state, which contains the item [S' -> S . [#]]. *)

(* [is_start_or_exit node] determines whether [node] is one of these
   two states and, if so, returns the corresponding start symbol [S]. *)

val is_start_or_exit: node -> Nonterminal.t option

(* This maps a node to its predecessors. *)

val predecessors: node -> node list

(* A view of the forward edges as a graph. *)

module ForwardEdges : sig
  type nonrec node = node
  type label = Symbol.t
  val foreach_outgoing_edge: node -> (label -> node -> unit) -> unit
end

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

(* [all_sources symbol] is the set of all sources of edges labeled
   with [symbol]. *)

val all_sources: Symbol.t -> NodeSet.t

(* [all_targets symbol] is the set of all targets of edges labeled
   with [symbol]. *)

val all_targets: Symbol.t -> NodeSet.t

(* [ntargets symbol] counts the number of distinct targets of edges
   labeled with [symbol]. If [symbol] is a non-terminal symbol [nt]
   then this is the number of branches in the [goto] function
   associated with [nt]. *)

val ntargets: Symbol.t -> int

(* [single_target symbol] requires [ntargets symbol = 1], and returns
   the single target of all edges labeled with [symbol]. *)

val single_target: Symbol.t -> node

(* Iteration over all nodes with conflicts. [conflicts f] invokes [f toks
   node] once for every node [node] with a conflict, where [toks] are the
   tokens involved in the conflicts at that node.

   If this function is invoked after conflicts have been resolved, then
   no conflicts are reported. *)

val conflicts: (TerminalSet.t -> node -> unit) -> unit

(* [conflict_tokens node] returns the set of tokens where [node] has a
   conflict.

   If this function is invoked after conflicts have been resolved, then
   no conflict tokens are reported. *)

val conflict_tokens: node -> TerminalSet.t

(* [has_eos_conflict node] indicates whether [node] has an end-of-stream
   conflict. If so, the list of productions and the lookahead tokens that are
   involved are returned.

   If this function is invoked after conflicts have been resolved, then
   no end-of-stream conflicts are reported. *)

val has_eos_conflict: node -> (Production.index list * TerminalSet.t) option

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
   [prod] might be reduced. It is an error to call this function before
   default conflict resolution has taken place. *)

val production_where: Production.index -> NodeSet.t
