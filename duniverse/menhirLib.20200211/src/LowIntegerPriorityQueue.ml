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

(* This module implements a simple-minded priority queue, under the assumption
   that priorities are low nonnegative integers. *)

module MyArray = ResizableArray
module MyStack = ResizableArray

type 'a t = {

  (* A priority queue is represented as a resizable array, indexed by
     priorities, of stacks (implemented as resizable arrays). There is no a
     priori bound on the size of the main array -- its size is increased if
     needed. It is up to the user to use priorities of reasonable
     magnitude. *)
  a: 'a MyStack.t MyArray.t;

  (* Index of lowest nonempty stack, if there is one; or lower (sub-optimal,
     but safe). If the queue is empty, [best] is arbitrary. *)
  mutable best: int;

  (* Current number of elements in the queue. Used in [remove] to stop the
     search for a nonempty bucket. *)
  mutable cardinal: int;

}

let create default =
  (* Set up the main array so that it initially has 16 priority levels and, whenever
     new levels are added, each of them is initialized with a fresh empty stack. The
     dummy stack is never accessed; it is used to fill empty physical slots in the
     main array. *)
  let dummy = MyStack.make_ 0 default in
  let a = MyArray.make 16 dummy (fun _ -> MyStack.make_ 1024 default) in
  { a; best = 0; cardinal = 0 }

let add q x priority =
  assert (0 <= priority);
  q.cardinal <- q.cardinal + 1;
  (* Grow the main array if necessary. *)
  if MyArray.length q.a <= priority then
    MyArray.resize q.a (priority + 1);
  (* Find out which stack we should push into. *)
  let xs = MyArray.get q.a priority in
  (* assert (xs != MyArray.default q.a); *)
  (* Push. *)
  MyStack.push xs x;
  (* Decrease [q.best], if necessary, so as not to miss the new element. In
     the special case of Dijkstra's algorithm or A*, this never happens. *)
  if priority < q.best then
    q.best <- priority

let is_empty q =
  q.cardinal = 0

let cardinal q =
  q.cardinal

let rec remove_nonempty q =
  (* Look for the next nonempty bucket. We know there is one. This may seem
     inefficient, because it is a linear search. However, in applications
     where [q.best] never decreases, the cumulated cost of this loop is the
     maximum priority ever used, which is good. *)
  let xs = MyArray.get q.a q.best in
  if MyStack.length xs = 0 then begin
    (* As noted below, [MyStack.pop] does not physically shrink the stack.
       When we find that a priority level has become empty, we physically
       empty it, so as to free the (possibly large) space that it takes up.
       This strategy is good when the client is Dijkstra's algorithm or A*. *)
    let dummy = MyArray.default q.a in
    MyArray.set q.a q.best dummy;
    q.best <- q.best + 1;
    remove_nonempty q
  end
  else begin
    q.cardinal <- q.cardinal - 1;
    Some (MyStack.pop xs)
    (* Note: [MyStack.pop] does not shrink the physical array underlying the
       stack. This is good, because we are likely to push new elements into
       this stack. *)
  end

let remove q =
  if q.cardinal = 0 then
    None
  else
    remove_nonempty q

let rec repeat q f =
  match remove q with
  | None ->
      ()
  | Some x ->
      f x;
      repeat q f
