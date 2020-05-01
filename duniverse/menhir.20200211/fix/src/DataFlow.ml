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

module Run
  (M : MINIMAL_IMPERATIVE_MAPS)
  (P : SEMI_LATTICE)
  (G : DATA_FLOW_GRAPH with type variable = M.key and type property = P.property)
= struct
  open P

  type variable = M.key

  (* A mapping of variables to properties. This mapping is initially empty. *)

  let properties =
    M.create()

  (* A set of dirty variables, whose outgoing transitions must be examined. *)

  (* The set of dirty variables is represented as a combination of a stack and
     a map of variables to Booleans. This map keeps track of which variables
     are in the stack and allows us to avoid pushing a variable onto the stack
     when it is already in the stack. (In principle, a map of variables to
     [unit] should suffice, but our minimal map API does not offer a [remove]
     function. Thus, we have to use a map of variables to Booleans.) *)

  let pending : variable Stack.t =
    Stack.create()

  let dirty : bool M.t =
    M.create()

  let is_dirty (x : variable) =
    try M.find x dirty with Not_found -> false

  let schedule (x : variable) =
    if not (is_dirty x) then begin
      M.add x true dirty;
      Stack.push x pending
    end

  (* [update x' p'] ensures that the property associated with the variable [x']
     is at least [p']. If this causes a change in the property at [x'], then
     [x] is scheduled or rescheduled. *)

  let update (x' : variable) (p' : property) =
    match M.find x' properties with
    | exception Not_found ->
        (* [x'] is newly discovered. *)
        M.add x' p' properties;
        schedule x'
    | p ->
        (* [x'] has been discovered earlier. *)
        if not (P.leq p' p) then begin
          (* [x'] is affected by this update and must itself be scheduled. *)
          M.add x' (P.join p' p) properties;
          schedule x'
        end

  (* [examine] examines a variable that has just been taken out of the stack.
     Its outgoing transitions are inspected and its successors are updated. *)

  let examine (x : variable) =
    (* [x] is dirty, so a property must have been associated with it. *)
    let p = try M.find x properties with Not_found -> assert false in
    G.foreach_successor x p update

  (* Populate the stack with the root variables. *)

  let () =
    G.foreach_root (fun x p ->
      M.add x p properties;
      schedule x
    )

  (* As long as the stack is nonempty, pop a variable and examine it. *)

  let () =
    try
      while true do
        let x = Stack.pop pending in
        M.add x false dirty;
        examine x
      done
    with Stack.Empty ->
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
