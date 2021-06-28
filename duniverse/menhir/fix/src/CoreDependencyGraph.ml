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

(* Using doubly-linked adjacency lists, one could implement [predecessors] in
   worst-case linear time with respect to the length of the output list,
   [set_successors] in worst-case linear time with respect to the length of
   the input list, and [clear_successors] in worst-case linear time with
   respect to the number of edges that are removed. We use a simpler
   implementation, based on singly-linked adjacency lists, with deferred
   removal of edges. It achieves the same complexity bounds, except
   [predecessors] only offers an amortized complexity bound. This is good
   enough for our purposes, and, in practice, is more efficient by a constant
   factor. This simplification was suggested by Arthur Charguéraud. *)

(* -------------------------------------------------------------------------- *)

(* Nodes and edges. *)

type 'data node = {

  (* The client information associated with this node. *)

  data: 'data;

  (* This node's incoming and outgoing edges. *)

  mutable outgoing: 'data edge list;
  mutable incoming: 'data edge list;

  (* A transient mark, always set to [false], except when checking
     against duplicate elements in a successor list. *)

  mutable marked: bool;

}

and 'data edge = {

  (* This edge's nodes. Edges are symmetric: source and destination are not
     distinguished. Thus, an edge appears both in the outgoing edge list of
     its source node and in the incoming edge list of its destination node.
     This allows edges to be easily marked as destroyed. *)

  node1: 'data node;
  node2: 'data node;

  (* Edges that are destroyed are marked as such, but are not immediately
     removed from the adjacency lists. *)

  mutable destroyed: bool;

}

(* -------------------------------------------------------------------------- *)

(* Node creation. *)

let create data = {
  data = data;
  outgoing = [];
  incoming = [];
  marked = false;
}

(* Data access. *)

let data node =
  node.data

(* [follow src edge] returns the node that is connected to [src] by [edge].
   Time complexity: constant. *)

let follow src edge =
  if edge.node1 == src then
    edge.node2
  else begin
    assert (edge.node2 == src);
    edge.node1
  end

(* The [predecessors] function removes edges that have been marked
   destroyed. The cost of removing these has already been paid for,
   so the amortized time complexity of [predecessors] is linear in
   the length of the output list. *)

let predecessors (node : 'data node) : 'data node list =
  let predecessors =
    List.filter (fun edge -> not edge.destroyed) node.incoming
  in
  node.incoming <- predecessors;
  List.map (follow node) predecessors

(* [link src dst] creates a new edge from [src] to [dst], together
   with its reverse edge. Time complexity: constant. *)

let link (src : 'data node) (dst : 'data node) =
  let edge = {
    node1 = src;
    node2 = dst;
    destroyed = false;
  } in
  src.outgoing <- edge :: src.outgoing;
  dst.incoming <- edge :: dst.incoming

let set_successors (src : 'data node) (dsts : 'data node list) =
  assert (src.outgoing = []);
  let rec loop = function
    | [] ->
        ()
    | dst :: dsts ->
        if dst.marked then
          loop dsts (* skip duplicate elements *)
        else begin
          dst.marked <- true;
          link src dst;
          loop dsts;
          dst.marked <- false
        end
  in
  loop dsts

let clear_successors node =
  List.iter (fun edge ->
    assert (not edge.destroyed);
    edge.destroyed <- true;
  ) node.outgoing;
  node.outgoing <- []
