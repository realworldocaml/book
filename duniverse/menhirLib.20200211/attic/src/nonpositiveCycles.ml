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

(* This module uses Floyd and Warshall's algorithm to detect whether a graph
   with integer-weighted edges contains a simple cycle of negative weight. *)

(* The algorithm runs in cubic time in the number of vertices. It may be
   worthwhile to first use Tarjan's algorithm to obtain the graph's strongly
   connected components, and use Floyd and Warshall's algorithm only on each
   component. *)

module Run (G : sig

  type node

  (* We assume each node has a unique index. Indices must range from
     $0$ to $n-1$, where $n$ is the number of nodes in the graph. *)

  val n: int
  val index: node -> int

  (* Iterating over a node's immediate successors. Edges are weighted. *)

  val successors: (int -> node -> unit) -> node -> unit

  (* Iterating over all nodes. *)

  val iter: (node -> unit) -> unit

end) = struct

  open G

  (* Integers with infinity. *)

  type distance =
    | Infinity
    | Finite of int

  let add d1 d2 =
    match d1, d2 with
    | Infinity, _
    | _, Infinity ->
        Infinity
    | Finite i1, Finite i2 ->
        Finite (i1 + i2)

  let min d1 d2 =
    match d1, d2 with
    | Infinity, d
    | d, Infinity ->
        d
    | Finite i1, Finite i2 ->
        Finite (min i1 i2)

  let le d1 d2 =
    match d1, d2 with
    | Infinity, Infinity ->
        true
    | Infinity, Finite _ ->
        false
    | Finite _, Infinity ->
        true
    | Finite i1, Finite i2 ->
        i1 <= i2

  (* Allocate and initialize a distance matrix. At allocation time, every entry
     is initialized to infinity. Then, we iterate over all edges, and copy them
     into the distance matrix. *)

  (* Note that, by default, [d.(i).(i)] is not initialized to zero: it is
     initialized to infinity. This is because we are looking for paths of
     non-zero length. In other words, we are computing a transitive closure,
     not a reflexive, transitive closure. *)

  let d =
    Array.init n (fun i ->
      Array.init n (fun j ->
        Infinity
      )
    )

  let () =
    iter (fun source ->
      successors (fun weight target ->
        (* We use a min operation, so the graph may be a multi-graph, that is,
           multiple edges between two nodes are permitted. *)
        let i = index source
        and j = index target in
        d.(i).(j) <- min (Finite weight) d.(i).(j)
      ) source
    )

  (* The algorithm. *)

  (* Stefan Hougardy notes that, in the presence of negative cycles, distances
     can grow exponentially fast (towards minus infinity), so there is a risk
     of overflow. To avoid this, one must check for negative cycles during the
     computation -- as opposed to waiting until the end. *)

  exception Detection

  let graph_has_nonpositive_simple_cycle : bool =
    try
      for k = 0 to n-1 do
        for i = 0 to n-1 do
          for j = 0 to n-1 do
            d.(i).(j) <- min d.(i).(j) (add d.(i).(k) d.(k).(j));
            if i = j && le d.(i).(j) (Finite 0) then
              raise Detection
          done
        done
      done;
      false
    with Detection ->
      true

end

