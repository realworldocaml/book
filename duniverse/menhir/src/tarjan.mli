(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module provides an implementation of Tarjan's algorithm for
   finding the strongly connected components of a graph.

   The algorithm runs when the functor is applied. Its complexity is
   $O(V+E)$, where $V$ is the number of vertices in the graph $G$, and
   $E$ is the number of edges. *)

module Run (G : sig

  type node

  (* We assume each node has a unique index. Indices must range from
     $0$ to $n-1$, where $n$ is the number of nodes in the graph. *)

  val n: int
  val index: node -> int

  (* Iterating over a node's immediate successors. *)

  val successors: (node -> unit) -> node -> unit

  (* Iterating over all nodes. *)

  val iter: (node -> unit) -> unit

end) : sig

  open G

  (* This function maps each node to a representative element of its
     component. *)

  val representative: node -> node

  (* This function maps each representative element to a list of all
     members of its component. Non-representative elements are mapped
     to an empty list. *)

  val scc: node -> node list

  (* [iter action] iterates over all components. For each component, the
     [action] function is applied to the component's representative element
     and to a list of the component's elements. (This must be a nonempty
     list.) The components are presented in topological order: that is, a
     component is examined before its successors are examined. *)

  val iter: (node -> node list -> unit) -> unit

  (* [rev_topological_iter action] iterates over all components. For each
     component, the [action] function is applied to the component's
     representative element and to a list of the component's elements. (This
     must be a nonempty list.) The components are presented in reverse
     topological order: that is, a component is examined after its successors
     have been examined. *)

  val rev_topological_iter: (node -> node list -> unit) -> unit

  (* [map action] iterates over all components in topological order, like
     [iter], and produces a list of results. *)

  val map: (node -> node list -> 'a) -> 'a list

  (* [rev_map action] iterates over all components in reverse topological
     order, like [rev_topological_iter], and produces a list of results in
     topological order. That is, the order of the traversal differs, but the
     order of the elements in the result list is the same as in [map]. *)

  val rev_map: (node -> node list -> 'a) -> 'a list

end
