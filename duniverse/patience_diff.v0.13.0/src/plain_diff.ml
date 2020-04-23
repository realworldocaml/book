(* This files comes from camlp5 (ocaml_src/lib/diff.ml). *)
(*
 * Copyright (c) 2007-2013, INRIA (Institut National de Recherches en
 * Informatique et Automatique). All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of INRIA, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software without
 *       specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY INRIA AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL INRIA AND
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
*)
(* $Id: diff.ml,v 1.2 2013-02-26 08:15:06 deraugla Exp $ *)
(* Parts of Code of GNU diff (diffseq.h and analyze.c) translated to OCaml and adjusted.

   Basic algorithm described by Eugene W.Myers in: "An O(ND) Difference Algorithm and Its
   Variations" *)

open Base

(* A partition is the midpoint of the shortest edit script for a specified portion of two
   vectors.

   [xmid, ymid] is the midpoint discovered. The diagonal number [xmid - ymid] equals the
   number of inserted elements minus the number of deleted elements (counting only
   elements before the midpoint).

   [lo_minimal] is true iff the minimal edit script for the left half of the partition is
   known; similarly for [hi_minimal].
*)
module Partition = struct
  type t =
    { xmid : int
    ; ymid : int
    ; lo_minimal : bool
    ; hi_minimal : bool
    }
end

(* We keep this file in a C-like style so that we can easily compare against the original
   C, in which we have great confidence. *)
(* Find the midpoint of the shortest edit script for a specified portion of the two
   vectors.

   Scan from the beginnings of the vectors, and simultaneously from the ends, doing a
   breadth-first search through the space of edit-sequence. When the two searches meet, we
   have found the midpoint of the shortest edit sequence.

   If [find_minimal] is true, find the minimal edit script regardless of expense.
   Otherwise, if the search is too expensive, use heuristics to stop the search and report
   a suboptimal answer.

   This function assumes that the first elements of the specified portions of the two
   vectors do not match, and likewise that the last elements do not match. The caller must
   trim matching elements from the beginning and end of the portions it is going to
   specify.

   If we return the "wrong" partitions, the worst this can do is cause suboptimal diff
   output. It cannot cause incorrect diff output. *)
let diag ~fd ~bd ~sh ~xv ~yv ~xoff ~xlim ~yoff ~ylim ~too_expensive ~find_minimal
  : Partition.t
  =
  let dmin = xoff - ylim (* minimum valid diagonal *) in
  let dmax = xlim - yoff (* maximum valid diagonal *) in
  let fmid = xoff - yoff (* center diagonal of forward search *) in
  let bmid = xlim - ylim (* center diagonal of backward search *) in
  (* southeast corner is on an odd diagonal w.r.t the northwest *)
  let odd = (fmid - bmid) land 1 <> 0 in
  (* [sh] is an offset that lets us use indices in [[-(m+1), n+1]]. *)
  fd.(sh + fmid) <- xoff;
  bd.(sh + bmid) <- xlim;
  With_return.with_return (fun ({ return } : Partition.t With_return.return) ->
    (* [c] is cost.
       [fmin], [fmax] are limits of the forward search.
       [bmin], [bmax] are limits of the backward search. *)
    let rec loop ~c ~fmin ~fmax ~bmin ~bmax =
      (* Extend the forward search by one edit step in each diagonal. *)
      let fmin =
        if fmin > dmin
        then (
          fd.(sh + fmin - 2) <- -1;
          fmin - 1)
        else fmin + 1
      in
      let fmax =
        if fmax < dmax
        then (
          fd.(sh + fmax + 2) <- -1;
          fmax + 1)
        else fmax - 1
      in
      (* [d] is the active diagonal. *)
      (let rec loop d =
         if d < fmin
         then ()
         else (
           let tlo = fd.(sh + d - 1) in
           let thi = fd.(sh + d + 1) in
           let x = if tlo >= thi then tlo + 1 else thi in
           let x, y =
             let rec loop ~xv ~yv ~xlim ~ylim ~x ~y =
               if x < xlim && y < ylim && phys_equal (xv x) (yv y)
               then loop ~xv ~yv ~xlim ~ylim ~x:(x + 1) ~y:(y + 1)
               else x, y
             in
             loop ~xv ~yv ~xlim ~ylim ~x ~y:(x - d)
           in
           fd.(sh + d) <- x;
           if odd && bmin <= d && d <= bmax && bd.(sh + d) <= fd.(sh + d)
           then return { xmid = x; ymid = y; lo_minimal = true; hi_minimal = true }
           else loop (d - 2))
       in
       loop fmax);
      (* Similarly extend the backward search. *)
      let bmin =
        if bmin > dmin
        then (
          bd.(sh + bmin - 2) <- Int.max_value;
          bmin - 1)
        else bmin + 1
      in
      let bmax =
        if bmax < dmax
        then (
          bd.(sh + bmax + 2) <- Int.max_value;
          bmax + 1)
        else bmax - 1
      in
      (let rec loop d =
         if d < bmin
         then ()
         else (
           let tlo = bd.(sh + d - 1) in
           let thi = bd.(sh + d + 1) in
           let x = if tlo < thi then tlo else thi - 1 in
           let x, y =
             let rec loop ~xv ~yv ~xoff ~yoff ~x ~y =
               if x > xoff && y > yoff && phys_equal (xv (x - 1)) (yv (y - 1))
               then loop ~xv ~yv ~xoff ~yoff ~x:(x - 1) ~y:(y - 1)
               else x, y
             in
             loop ~xv ~yv ~xoff ~yoff ~x ~y:(x - d)
           in
           bd.(sh + d) <- x;
           if (not odd) && fmin <= d && d <= fmax && bd.(sh + d) <= fd.(sh + d)
           then return { xmid = x; ymid = y; lo_minimal = true; hi_minimal = true }
           else loop (d - 2))
       in
       loop bmax);
      (* Heuristic: if we've gone well beyond the call of duty, give up and report halfway
         between our best results so far. *)
      if (not find_minimal) && c >= too_expensive
      then (
        (* Find forward diagonal that maximizes [x + y]. *)
        let fxybest, fxbest =
          let rec loop ~d ~fxybest ~fxbest =
            if d < fmin
            then fxybest, fxbest
            else (
              let x = Int.min fd.(sh + d) xlim in
              let y = x - d in
              let x, y = if ylim < y then ylim + d, ylim else x, y in
              let fxybest, fxbest =
                if fxybest < x + y then x + y, x else fxybest, fxbest
              in
              loop ~d:(d - 2) ~fxybest ~fxbest)
          in
          loop ~d:fmax ~fxybest:(-1) ~fxbest:fmax
        in
        (* Find backward diagonal that minimizes [x + y]. *)
        let bxybest, bxbest =
          let rec loop ~d ~bxybest ~bxbest =
            if d < bmin
            then bxybest, bxbest
            else (
              let x = Int.max xoff bd.(sh + d) in
              let y = x - d in
              let x, y = if y < yoff then yoff + d, yoff else x, y in
              let bxybest, bxbest =
                if x + y < bxybest then x + y, x else bxybest, bxbest
              in
              loop ~d:(d - 2) ~bxybest ~bxbest)
          in
          loop ~d:bmax ~bxybest:Int.max_value ~bxbest:bmax
        in
        if xlim + ylim - bxybest < fxybest - (xoff + yoff)
        then
          return
            { xmid = fxbest
            ; ymid = fxybest - fxbest
            ; lo_minimal = true
            ; hi_minimal = false
            }
        else
          return
            { xmid = bxbest
            ; ymid = bxybest - bxbest
            ; lo_minimal = false
            ; hi_minimal = true
            })
      else loop ~c:(c + 1) ~fmin ~fmax ~bmin ~bmax
    in
    loop ~c:1 ~fmin:fmid ~fmax:fmid ~bmin:bmid ~bmax:bmid)
;;

let diff_loop ~cutoff a ai b bi n m =
  let fd = Array.create ~len:(n + m + 3) 0 in
  let bd = Array.create ~len:(n + m + 3) 0 in
  let sh = m + 1 in
  let too_expensive =
    match cutoff with
    | Some c -> c
    | None ->
      let diags = n + m + 3 in
      let rec loop diags too_expensive =
        if diags = 0 then too_expensive else loop (diags asr 2) (too_expensive lsl 1)
      in
      Int.max 4096 (loop diags 1)
  in
  let xvec i = a.(ai.(i)) in
  let yvec j = b.(bi.(j)) in
  let chng1 = Array.create ~len:(Array.length a) true in
  let chng2 = Array.create ~len:(Array.length b) true in
  for i = 0 to n - 1 do
    chng1.(ai.(i)) <- false
  done;
  for j = 0 to m - 1 do
    chng2.(bi.(j)) <- false
  done;
  let rec loop ~xoff ~xlim ~yoff ~ylim ~find_minimal =
    let xoff, yoff =
      let rec loop ~xoff ~yoff =
        if xoff < xlim && yoff < ylim && phys_equal (xvec xoff) (yvec yoff)
        then loop ~xoff:(xoff + 1) ~yoff:(yoff + 1)
        else xoff, yoff
      in
      loop ~xoff ~yoff
    in
    let xlim, ylim =
      let rec loop ~xlim ~ylim =
        if xlim > xoff && ylim > yoff && phys_equal (xvec (xlim - 1)) (yvec (ylim - 1))
        then loop ~xlim:(xlim - 1) ~ylim:(ylim - 1)
        else xlim, ylim
      in
      loop ~xlim ~ylim
    in
    if xoff = xlim
    then
      for y = yoff to ylim - 1 do
        chng2.(bi.(y)) <- true
      done
    else if yoff = ylim
    then
      for x = xoff to xlim - 1 do
        chng1.(ai.(x)) <- true
      done
    else (
      let { Partition.xmid; ymid; lo_minimal; hi_minimal } =
        diag
          ~fd
          ~bd
          ~sh
          ~xv:xvec
          ~yv:yvec
          ~xoff
          ~xlim
          ~yoff
          ~ylim
          ~too_expensive
          ~find_minimal
      in
      loop ~xoff ~xlim:xmid ~yoff ~ylim:ymid ~find_minimal:lo_minimal;
      loop ~xoff:xmid ~xlim ~yoff:ymid ~ylim ~find_minimal:hi_minimal)
  in
  loop ~xoff:0 ~xlim:n ~yoff:0 ~ylim:m ~find_minimal:false;
  chng1, chng2
;;

(* [make_indexer a b] returns an array of the indices of items of [a] which are also
   present in [b]; this way, the main algorithm can skip items which, anyway, are
   different. This improves the speed much.  At the same time, this function updates the
   items of [a] and [b] so that all equal items point to the same unique item.  All item
   comparisons in the main algorithm can therefore be done with [phys_equal] instead of
   [=], which can improve speed much. *)
let make_indexer hashable a b =
  let n = Array.length a in
  let htb = Hashtbl.create hashable ~size:(10 * Array.length b) in
  Array.iteri
    ~f:(fun i e ->
      match Hashtbl.find htb e with
      | Some v -> b.(i) <- v
      | None -> Hashtbl.add_exn htb ~key:e ~data:e)
    b;
  let ai = Array.create ~len:n 0 in
  let k =
    let rec loop i k =
      if i = n
      then k
      else (
        let k =
          match Hashtbl.find htb a.(i) with
          | Some v ->
            a.(i) <- v;
            ai.(k) <- i;
            k + 1
          | None -> k
        in
        loop (i + 1) k)
    in
    loop 0 0
  in
  Array.sub ai ~pos:0 ~len:k
;;

let f ~cutoff ~hashable a b =
  let ai = make_indexer hashable a b in
  let bi = make_indexer hashable b a in
  let n = Array.length ai in
  let m = Array.length bi in
  diff_loop ~cutoff a ai b bi n m
;;

let iter_matches ?cutoff ~f:ff ~hashable a b =
  let d1, d2 = f ~cutoff ~hashable a b in
  let rec aux i1 i2 =
    if i1 >= Array.length d1 || i2 >= Array.length d2
    then ()
    else if not d1.(i1)
    then
      if not d2.(i2)
      then (
        ff (i1, i2);
        aux (i1 + 1) (i2 + 1))
      else aux i1 (i2 + 1)
    else if not d2.(i2)
    then aux (i1 + 1) i2
    else aux (i1 + 1) (i2 + 1)
  in
  aux 0 0
;;
