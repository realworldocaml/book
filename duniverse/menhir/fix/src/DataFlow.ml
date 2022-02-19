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

open Sigs

(* Such a data flow analysis problem could also be solved by the generic least
   fixed point computation algorithm [Fix.Make.lfp]. However, such an approach
   would be less efficient, as (1) it would require reversing the graph first,
   so to have access to predecessors; (2) whenever a dirty node is examined,
   the contributions of all of its predecessors would be recomputed and
   joined, whereas the forward data flow analysis algorithm pushes information
   from a dirty node to its successors, thereby avoiding recomputation along
   edges whose source is not dirty; (3) the generic algorithm performs dynamic
   discovery of dependencies, whereas in this situation, all dependencies are
   explicitly provided by the user. *)

(* We require a minimal semi-lattice, equipped with a [leq_join] operation, as
   opposed to a semi-lattice, which offers separate [leq] and [join]
   operations. Although [leq_join] is less powerful, it is sufficient for our
   purposes, and is potentially more efficient than the sequence of [leq]
   [join]. *)

module Run
  (M : MINIMAL_IMPERATIVE_MAPS)
  (P : MINIMAL_SEMI_LATTICE)
  (G : DATA_FLOW_GRAPH with type variable = M.key and type property = P.property)
= struct
  open P

  type variable = M.key

  (* A mapping of variables to properties. This mapping is initially empty. *)

  let properties =
    M.create()

  (* A set of dirty variables, whose outgoing transitions must be examined. *)

  (* The set of dirty variables is represented as a combination of a queue and
     a map of variables to Booleans. This map keeps track of which variables
     are in the queue and allows us to avoid inserting a variable into the queue
     when it is already in the queue. (In principle, a map of variables to
     [unit] should suffice, but our minimal map API does not offer a [remove]
     function. Thus, we have to use a map of variables to Booleans.) *)

  (* A FIFO queue is preferable to a LIFO stack. It leads to a breadth-first
     traversal. In the absence of cycles, in particular, this guarantees that
     a node is examined only after its predecessors have been examined.
     Compared to (say) a depth-first traversal, this allows more accurate
     properties to be computed in a single iteration, therefore reduces the
     number of iterations required in order to reach a fixed point. *)

  let pending : variable Queue.t =
    Queue.create()

  let dirty : bool M.t =
    M.create()

  let is_dirty (x : variable) =
    try M.find x dirty with Not_found -> false

  let schedule (x : variable) =
    if not (is_dirty x) then begin
      M.add x true dirty;
      Queue.add x pending
    end

  (* [update x' p'] ensures that the property associated with the variable [x']
     is at least [p']. If this causes a change in the property at [x'], then
     [x'] is scheduled or rescheduled. *)

  let update (x' : variable) (p' : property) =
    match M.find x' properties with
    | exception Not_found ->
        (* [x'] is newly discovered. *)
        M.add x' p' properties;
        schedule x'
    | p ->
        (* [x'] has been discovered earlier. *)
        let p'' = P.leq_join p' p in
        if p'' != p then begin
          (* The failure of the physical equality test [p'' == p] implies that
             [P.leq p' p] does not hold. Thus, [x'] is affected by this update
             and must itself be scheduled. *)
          M.add x' p'' properties;
          schedule x'
        end

  (* [examine] examines a variable that has just been taken out of the queue.
     Its outgoing transitions are inspected and its successors are updated. *)

  let examine (x : variable) =
    (* [x] is dirty, so a property must have been associated with it. *)
    let p = try M.find x properties with Not_found -> assert false in
    G.foreach_successor x p update

  (* Populate the queue with the root variables. *)

  (* Our use of [update] here means that it is permitted for [foreach_root]
     to seed several properties at a single root. *)

  let () =
    G.foreach_root update

  (* As long as the queue is nonempty, extract a variable and examine it. *)

  let () =
    try
      while true do
        let x = Queue.take pending in
        M.add x false dirty;
        examine x
      done
    with Queue.Empty ->
      ()

  (* Expose the solution. *)

  type property = P.property option

  let solution x =
    try
      Some (M.find x properties)
    with Not_found ->
      None

end

module ForOrderedType (T : OrderedType) =
  Run(Glue.PersistentMapsToImperativeMaps(Map.Make(T)))

module ForHashedType (T : HashedType) =
  Run(Glue.HashTablesAsImperativeMaps(T))

module ForType (T : TYPE) =
  ForHashedType(Glue.TrivialHashedType(T))

module ForIntSegment (K : sig val n: int end) =
  Run(Glue.ArraysAsImperativeMaps(K))

(* [ForCustomMaps] is a forward data flow analysis that is tuned for
   performance. *)

module ForCustomMaps
  (P : MINIMAL_SEMI_LATTICE)
  (G : DATA_FLOW_GRAPH with type property := P.property)
  (V : ARRAY with type key := G.variable and type value := P.property)
  (B : ARRAY with type key := G.variable and type value := bool)
    : sig end
= struct
  open P
  open G

  (* Compared to [Queue], [CompactQueue] is significantly faster and consumes
     less memory. *)

  let pending =
    CompactQueue.create ()

  (* The queue stores a set of dirty variables, whose outgoing transitions
     must be examined. The map [B] records whether a variable is currently
     queued. *)

  let schedule (x : variable) =
    if not (B.get x) then begin
      B.set x true;
      CompactQueue.add x pending
    end

  (* [update x' p'] ensures that the property associated with the variable [x']
     is at least [p']. If this causes a change in the property at [x'], then
     [x'] is scheduled or rescheduled. *)

  let update (x' : variable) (p' : property) =
    let p = V.get x' in
    let p'' = P.leq_join p' p in
    if p'' != p then begin
      (* The failure of the physical equality test [p'' == p] implies that
         [P.leq p' p] does not hold. Thus, [x'] is affected by this update
         and must itself be scheduled. *)
      V.set x' p'';
      schedule x'
    end

  (* [examine] examines a variable that has just been taken out of the queue.
     Its outgoing transitions are inspected and its successors are updated. *)

  let examine (x : variable) =
    let p = V.get x in
    G.foreach_successor x p update

  (* Populate the queue with the root variables. *)

  (* Our use of [update] here means that it is permitted for [foreach_root]
     to seed several properties at a single root. *)

  let () =
    G.foreach_root update

  (* As long as the queue is nonempty, take a variable and examine it. *)

  let () =
    try
      while true do
        let x = CompactQueue.take pending in
        B.set x false;
        examine x
      done
    with CompactQueue.Empty ->
      ()

end
