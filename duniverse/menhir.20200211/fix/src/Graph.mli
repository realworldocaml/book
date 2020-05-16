(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(* This module provides a data structure for maintaining and modifying
   a directed graph. Each node is allowed to carry a piece of client
   data. There are functions for creating a new node, looking up a
   node's data, looking up a node's predecessors, and setting or
   clearing a node's successors (all at once). *)

type 'data node

(* [create data] creates a new node, with no incident edges, with
   client information [data]. Time complexity: constant. *)

val create: 'data -> 'data node

(* [data node] returns the client information associated with
   the node [node]. Time complexity: constant. *)

val data: 'data node -> 'data

(* [predecessors node] returns a list of [node]'s predecessors.
   Amortized time complexity: linear in the length of the output list. *)

val predecessors: 'data node -> 'data node list

(* [set_successors src dsts] creates an edge from the node [src] to
   each of the nodes in the list [dsts]. Duplicate elements in the
   list [dsts] are removed, so that no duplicate edges are created. It
   is assumed that [src] initially has no successors. Time complexity:
   linear in the length of the input list. *)

val set_successors: 'data node -> 'data node list -> unit

(* [clear_successors node] removes all of [node]'s outgoing edges.
   Time complexity: linear in the number of edges that are removed. *)

val clear_successors: 'data node -> unit
