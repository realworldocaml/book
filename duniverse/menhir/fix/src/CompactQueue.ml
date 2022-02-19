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

(* A queue [q] is represented as a circular array of elements, [q.buffer]. The
   slots whose logical index lies between [q.first] (inclusive) and [q.first +
   q.size] (exclusive) contain the elements of the queue. A logical index must
   be normalized modulo [n], where [n] is the length of the array [buffer], to
   obtain a physical index. *)

(* We maintain the following invariants:

   - [n] is zero or a power of two, so as to speed up the [modulo] operation.

   - [0 <= q.size <= n] holds. *)

type 'a t = {

  (* The array that stores the elements (as well as empty slots). *)
  mutable buffer: 'a array;

  (* The logical index of the first element. *)
  mutable first: int;

    (* The number of elements. *)
  mutable size: int;

}

(* The following code iterates on all queued elements:

   let n = Array.length q.buffer in
   for i = 0 to q.size - 1 do
     f q.buffer.((q.first + i) mod n)
   done

   Because [n] is a power of 2, this is equivalent to:

   let n = Array.length q.buffer in
   for i = 0 to q.size - 1 do
     f q.buffer.((q.first + i) land (n - 1))
   done

 *)

(* The buffer is initially an empty array. Another array is allocated when an
   element is inserted. *)

let create () = {
  buffer = [||];
  first = 0;
  size = 0;
}

let is_empty q =
  q.size = 0

let add x q =
  let buffer = q.buffer in
  let n = Array.length buffer in
  if q.size < n then begin
    (* The queue still has room left. *)
    buffer.((q.first + q.size) land (n - 1)) <- x;
    q.size <- q.size + 1;
  end
  else if n > 0 then begin
    (* The buffer is nonempty, and is full. *)
    (* Allocate a new buffer that is twice larger. *)
    let buffer' = Array.make (n * 2) x in
    (* Move all existing elements. *)
    let first = q.first land (n - 1) in
    Array.blit buffer first buffer' 0 (n - first);
    Array.blit buffer 0 buffer' (n - first) first;
    q.buffer <- buffer';
    q.first <- 0;
    q.size <- n + 1;
  end
  else begin
    (* The queue has a buffer of size 0. Allocate an array. *)
    q.buffer <- Array.make 8 x;
    q.size <- 1;
  end

exception Empty

let take q =
  if q.size = 0 then
    raise Empty
  else begin
    q.size <- q.size - 1;
    let n = Array.length q.buffer in
    let result = q.buffer.(q.first land (n - 1)) in
    q.first <- q.first + 1;
    result
  end
