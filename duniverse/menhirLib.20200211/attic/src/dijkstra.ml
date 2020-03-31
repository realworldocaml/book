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

(* This module implements Dijkstra's algorithm over a graph with labeled
   weighted edges. (We assume the weight is contained in the label.) *)

module Make (G : sig

  (* This is the type of graph vertices. *)

  type vertex
  include Hashtbl.HashedType with type t := vertex

  (* This is the type of graph labels. *)

  type label

  (* The source vertex (or vertices). *)

  val sources: (vertex -> unit) -> unit

  (* The weighted outgoing edges of a vertex. *)

  val successors: vertex -> (label -> int -> vertex -> unit) -> unit

end) = struct
  open G

  (* The priority queue contains triples of a total weight, a vertex,
     and the discovery path for this vertex -- a list of labels. *)

  module Element = struct
    type t = int * vertex * label list
    let compare (w1, _, _) (w2, _, _) =
      Pervasives.compare w2 w1 (* switched because [Heap] uses the wrong convention *)
  end

  module PQ =
    Heap.Imperative(Element)

  (* A hash table maps vertices to distances. Another table maps vertices to unit
     values, indicating whether this distance is final. *)

  module H =
    Hashtbl.Make(struct include G type t = vertex end)

  let search f =

    (* Create the data structures. *)
    let queue = PQ.create 1024 in
    let dist : int H.t = H.create 1023 in
    let fixed : unit H.t = H.create 1023 in
    (* A handy accessor. *)
    let distance v =
      try H.find dist v with Not_found -> max_int
    in
    (* Insert the initial vertices. *)
    sources (fun source ->
      PQ.add queue (0, source, []);
      H.add dist source 0
    );
    (* As long as the queue is nonempty, ... *)
    while not (PQ.is_empty queue) do
      (* Extract one vertex. *)
      let (w, v, p) = PQ.pop_maximum queue in
      (* Check if this vertex is final already. If so, nothing to do. *)
      if not (H.mem fixed v) then begin
        (* Mark it final. *)
        H.add fixed v ();
        (* Let the client know about it. *)
        f (w, v, p);
        (* Examine its outgoing edges. *)
        successors v (fun label weight v' ->
          let w' = weight + w in
          if w' < distance v' then begin
            assert (not (H.mem fixed v'));
            H.replace dist v' w';
            PQ.add queue (w', v', label :: p)
          end
        )
      end
    done;
    distance

end

