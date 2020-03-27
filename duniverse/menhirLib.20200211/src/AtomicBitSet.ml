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

(* This module offers bitsets that fit within an OCaml integer. This can be
   used to represent sets of integers in the semi-open interval [0, bound),
   where [bound] is [Sys.word_size - 1], that is, usually 63. *)

type t =
  int

type element =
  int

let bound =
  Sys.word_size - 1

(* -------------------------------------------------------------------------- *)

(* [bit i] is [2^i]. *)

let bit i =
  assert (0 <= i && i < bound);
  1 lsl i

(* -------------------------------------------------------------------------- *)

(* [tib x] is the base-2 logarithm of [x]. We may assume that [x] is a power
   of two, that is, a single bit is set. The function [tib] is so named
   because it is the inverse of [bit]: [tib (bit i) = i]. *)

(* It would be nice if we could use gcc's __builtin_clz to do this.
   See caml_z.c in the library zarith. *)

(* The following code is based on Jean-Christophe Filliâtre's Bitset. *)

let log2 =
  Array.make 255 0
let () =
  for i = 0 to 7 do log2.(bit i) <- i done

let tib16 accu x =
  if x land 0xFF = 0 then
    accu + 8 + log2.(x lsr 8)
  else
    accu + log2.(x)

let tib32 accu x =
  if x land 0xFFFF = 0 then
    tib16 (accu + 16) (x lsr 16)
  else
    tib16 accu x

let ffffffff =
  (0xffff lsl 16) lor 0xffff
  (* We cannot use the literal 0xffffffff because the OCaml compiler will
     reject it when compiling for a 32-bit machine. *)

let tib64 x =
  if x land ffffffff = 0 then
    tib32 32 (x lsr 32)
  else
    tib32 0 x

let tib x =
  match Sys.word_size with
  | 32 -> tib32 0 x
  | 64 -> tib64 x
  | _ -> assert false

let () =
  (* A sanity check, executed once at startup time. *)
  for i = 0 to bound - 1 do assert (tib (bit i) = i) done

(* -------------------------------------------------------------------------- *)

(* [pop x] is the population count, that is, the number of bits that are set
   in [x]. *)

(* Again, it would be nice if we could use gcc's __builtin_popcount. *)

let b0 = 0x55555555
let b1 = 0x33333333
let b2 = 0xf0f0f0f

let pop32 x =
  (* Count the bits inside each byte, in parallel. *)
  (* https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel *)
  let x = x         - (x lsr 1) land b0 in
  let x = x land b1 + (x lsr 2) land b1 in
  let x = x land b2 + (x lsr 4) land b2 in
  (* Add up the four counts found in the four bytes. Each of these counts is
     at most 8, so the sum is at most 32, and fits in a byte. *)
  let x = x + x lsr 8 in
  let x = x + x lsr 16 in
  x land 0xff

let pop64 x =
  pop32 x + pop32 (x lsr 32)
    (* Because [pop32] examines only the lower 32 bits, we pass [x] [pop32]
       without bothering to mask off the higher 32 bits. *)

let pop x =
  match Sys.word_size with
  | 32 -> pop32 x
  | 64 -> pop64 x
  | _ -> assert false

(* -------------------------------------------------------------------------- *)

(* Operations. *)

let empty =
  0

let is_empty s =
  s = 0

let singleton =
  bit

let add i s =
  (bit i) lor s

let remove i s =
  (lnot (bit i)) land s

let rec fold_delta delta f s accu =
  if s = 0 then
    accu
  else
    let x = s land (-s) in
    let s = s lxor x in (* or: s - x *)
    let accu = f (delta + tib x) accu in
    fold_delta delta f s accu

let rec iter_delta delta f s =
  if s <> 0 then
    let x = s land (-s) in
    let s = s lxor x in (* or: s - x *)
    f (delta + tib x);
    iter_delta delta f s

let rec fold f s accu =
  if s = 0 then
    accu
  else
    let x = s land (-s) in
    let s = s lxor x in (* or: s - x *)
    let accu = f (tib x) accu in
    fold f s accu

let rec iter f s =
  if s <> 0 then
    let x = s land (-s) in
    let s = s lxor x in (* or: s - x *)
    f (tib x);
    iter f s

let is_singleton s =
  (* Test whether only one bit is set in [ss]. We do this by turning
     off the rightmost bit, then comparing to zero. *)
  s land (s - 1) = 0

let cardinal s =
  pop s
  (* or: fold (fun _ m -> m + 1) s 0 *)

let elements s =
  (* Note: the list is produced in decreasing order. *)
  fold (fun tl hd -> tl :: hd) s []

let subset s1 s2 =
  s1 land s2 = s1

let mem i s =
  subset (singleton i) s

let union s1 s2 =
  s1 lor s2

let inter s1 s2 =
  s1 land s2

let minimum s =
  if s = 0 then
    raise Not_found
  else
    let x = s land (-s) in
    tib x

let choose =
  minimum

let compare =
  compare (* this is [Generic.compare] *)

let equal s1 s2 =
  s1 = s2

let disjoint s1 s2 =
  is_empty (inter s1 s2)
