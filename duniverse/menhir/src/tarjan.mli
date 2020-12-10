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

  (* This function maps each node to a representative element of its strongly connected component. *)

  val representative: node -> node

  (* This function maps each representative element to a list of all
     members of its strongly connected component. Non-representative
     elements are mapped to an empty list. *)

  val scc: node -> node list

  (* [iter action] allows iterating over all strongly connected
     components. For each component, the [action] function is applied
     to the representative element and to a (non-empty) list of all
     elements. *)

  val iter: (node -> node list -> unit) -> unit

end

