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

(* Suppose [s] is a state that carries an outgoing edge labeled with a
   non-terminal symbol [nt]. We are interested in finding out how this edge
   can be taken. In order to do that, we must determine how, by starting in
   [s], one can follow a path that corresponds to (the right-hand side of) a
   production [prod] associated with [nt]. There are in general several such
   productions. The paths that they determine in the automaton form a "star".
   We represent the star rooted at [s] as a trie.

   A point in a trie (that is, a sub-trie) tells us where we come from, where
   we are, and which production(s) we are hoping to reduce in the future. *)

(* This module depends on [Grammar], [Lr1], [Default]: that is, we assume that
   the automaton has been fully constructed. It is used by [LRijkstra]. *)

module Make (X : sig end) : sig

  type trie

  (* [stars f] constructs the trie rooted at every state [s]. (There is one
     branch for every production [prod] associated with every non-terminal
     symbol [nt] for which [s] carries an outgoing edge.) If this trie [t] is
     nontrivial (i.e., it has at least one branch, leading to a state where a
     production can be reduced), then [f s t] is invoked. *)
  val stars: (Lr1.node -> trie -> unit) -> unit

  (* After [stars] has been called, [size (Lr1.number s)] reports the size
     of the trie that has been constructed for state [s]. *)
  val size: int -> int

  (* After [stars] has been called, [total_size()] reports the total size of
     the tries that have been constructed. *)
  val total_size: unit -> int

  (* Every (sub-)trie has a unique identity. (One can think of it as its
     address.) [compare] compares the identity of two tries. This can be
     used, e.g., to set up a map whose keys are tries. *)
  val compare: trie -> trie -> int

  (* [source t] returns the source state of the (sub-)trie [t]. This is
     the root of the star of which [t] is a sub-trie. In other words, this
     tells us "where we come from". *)
  val source: trie -> Lr1.node

  (* [current t] returns the current state of the (sub-)trie [t]. This is
     the root of the sub-trie [t]. In other words, this tells us "where
     we are". *)
  val current: trie -> Lr1.node

  (* [accepts prod t] tells whether the current state of the trie [t] is
     the end of a branch associated with production [prod]. If so, this
     means that we have successfully followed a path that corresponds to
     the right-hand side of production [prod]. *)
  val accepts: Production.index -> trie -> bool

  (* [step sym t] is the immediate sub-trie of [t] along the symbol [sym].
     This function raises [Not_found] if [t] has no child labeled [sym]. *)
  val step: Symbol.t -> trie -> trie

  (* [verbose()] outputs debugging & performance information. *)
  val verbose: unit -> unit

  (* Since every (sub-)trie has a unique identity, its identity can serve
     as a unique integer code for this (sub-)trie. We allow this conversion,
     both ways. This mechanism is used only as a way of saving space in the
     encoding of facts. *)
  val encode: trie -> int
  val decode: int -> trie

end
