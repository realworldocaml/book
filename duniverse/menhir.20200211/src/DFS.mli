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

(* A generic implementation of depth-first search. *)

(* The graph [G] must be equipped with ways of iterating over the outoing
   edges of a node and over the root notes. Edges can be labeled. If no labels
   are needed, then the type [label] should be defined as [unit]. *)

(* The module [M] must offer a mechanism for marking a node and testing whether
   a node is marked. The functors [MarkSet] and [MarkArray] (below) can help
   implement it. *)

(* The function [D.discover] is invoked at most once per node, when this node
   is newly discovered (after this node has been marked and before its
   outgoing edges are traversed). The function [D.traverse] is invoked at most
   once per edge, when this edge is traversed. *)

(* The functor application [Run(G)(M)(D)] performs the search. No result is
   returned. *)

module Run
(G : sig
  type node
  type label
  val foreach_outgoing_edge: node -> (label -> node -> unit) -> unit
  val foreach_root: (node -> unit) -> unit
end)
(M : sig
  val mark: G.node -> unit
  val is_marked: G.node -> bool
end)
(D : sig
  val discover: G.node -> unit
  val traverse: G.node -> G.label -> G.node -> unit
end)
: sig end

(* The module [MarkSet(S)] provides a fresh marking mechanism for elements of
   type [S.elt], where [S] is a set implementation. The functions [mark] and
   [is_marked] allow marking an element and testing whether an element is
   marked. The function [marked] returns the set of all marked elements. *)

module MarkSet (S : Set.S) : sig
  val mark: S.elt -> unit
  val is_marked: S.elt -> bool
  val marked: unit -> S.t
end

(* The module [MarkArray(S)] provides a fresh marking mechanism for nodes of
   type [G.node], where [G] is a graph whose nodes are numbered. The functions
   [mark] and [is_marked] allow marking a node and testing whether a node is
   marked. The function [marked] returns an array of marks. *)

module MarkArray (G : sig
  type node
  val n: int
  val number: node -> int
end) : sig
  val mark: G.node -> unit
  val is_marked: G.node -> bool
  val marked: unit -> bool array
end
