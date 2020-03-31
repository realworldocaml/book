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

(* $Id:$ *)

module type Ordered = sig
  type t
  val compare : t -> t -> int
end

exception EmptyHeap

(*s Imperative implementation *)

module Imperative(X : Ordered) = struct

  (* The heap is encoded in the array [data], where elements are stored
     from [0] to [size - 1]. From an element stored at [i], the left
     (resp. right) subtree, if any, is rooted at [2*i+1] (resp. [2*i+2]). *)

  type t = { mutable size : int; mutable data : X.t array }

  (* When [create n] is called, we cannot allocate the array, since there is
     no known value of type [X.t]; we'll wait for the first addition to
     do it, and we remember this situation with a negative size. *)

  let create n =
    if n <= 0 then invalid_arg "create";
    { size = -n; data = [||] }

  let is_empty h = h.size <= 0

  (* [resize] doubles the size of [data] *)

  let resize h =
    let n = h.size in
    assert (n > 0);
    let n' = 2 * n in
    let d = h.data in
    let d' = Array.make n' d.(0) in
    Array.blit d 0 d' 0 n;
    h.data <- d'

  let add h x =
    (* first addition: we allocate the array *)
    if h.size < 0 then begin
      h.data <- Array.make (- h.size) x; h.size <- 0
    end;
    let n = h.size in
    (* resizing if needed *)
    if n == Array.length h.data then resize h;
    let d = h.data in
    (* moving [x] up in the heap *)
    let rec moveup i =
      let fi = (i - 1) / 2 in
      if i > 0 && X.compare d.(fi) x < 0 then begin
        d.(i) <- d.(fi);
        moveup fi
      end else
        d.(i) <- x
    in
    moveup n;
    h.size <- n + 1

  let maximum h =
    if h.size <= 0 then raise EmptyHeap;
    h.data.(0)

  let remove h =
    if h.size <= 0 then raise EmptyHeap;
    let n = h.size - 1 in
    h.size <- n;
    let d = h.data in
    let x = d.(n) in
    (* moving [x] down in the heap *)
    let rec movedown i =
      let j = 2 * i + 1 in
      if j < n then
        let j =
          let j' = j + 1 in
          if j' < n && X.compare d.(j') d.(j) > 0 then j' else j
        in
        if X.compare d.(j) x > 0 then begin
          d.(i) <- d.(j);
          movedown j
        end else
          d.(i) <- x
      else
        d.(i) <- x
    in
    movedown 0

  let pop_maximum h = let m = maximum h in remove h; m

  let iter f h =
    let d = h.data in
    for i = 0 to h.size - 1 do f d.(i) done

  let fold f h x0 =
    let n = h.size in
    let d = h.data in
    let rec foldrec x i =
      if i >= n then x else foldrec (f d.(i) x) (succ i)
    in
    foldrec x0 0

  let cardinal h =
    if h.size <= 0 then 0 else h.size

  let repeat h f =
    while h.size > 0 do
      let x = pop_maximum h in
      f x
    done

end


