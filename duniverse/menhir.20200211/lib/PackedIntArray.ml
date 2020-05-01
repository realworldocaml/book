(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(* A packed integer array is represented as a pair of an integer [k] and
   a string [s]. The integer [k] is the number of bits per integer that we
   use. The string [s] is just an array of bits, which is read in 8-bit
   chunks. *)

(* The ocaml programming language treats string literals and array literals
   in slightly different ways: the former are statically allocated, while
   the latter are dynamically allocated. (This is rather arbitrary.) In the
   context of Menhir's table-based back-end, where compact, immutable
   integer arrays are needed, ocaml strings are preferable to ocaml arrays. *)

type t =
  int * string

(* The magnitude [k] of an integer [v] is the number of bits required
   to represent [v]. It is rounded up to the nearest power of two, so
   that [k] divides [Sys.word_size]. *)

let magnitude (v : int) =
  if v < 0 then
    Sys.word_size
  else
    let rec check k max = (* [max] equals [2^k] *)
      if (max <= 0) || (v < max) then
        k
          (* if [max] just overflew, then [v] requires a full ocaml
             integer, and [k] is the number of bits in an ocaml integer
             plus one, that is, [Sys.word_size]. *)
      else
        check (2 * k) (max * max)
    in
    check 1 2

(* [pack a] turns an array of integers into a packed integer array. *)

(* Because the sign bit is the most significant bit, the magnitude of
   any negative number is the word size. In other words, [pack] does
   not achieve any space savings as soon as [a] contains any negative
   numbers, even if they are ``small''. *)

let pack (a : int array) : t =

  let m = Array.length a in

  (* Compute the maximum magnitude of the array elements. This tells
     us how many bits per element we are going to use. *)

  let k =
    Array.fold_left (fun k v ->
      max k (magnitude v)
    ) 1 a
  in

  (* Because access to ocaml strings is performed on an 8-bit basis,
     two cases arise. If [k] is less than 8, then we can pack multiple
     array entries into a single character. If [k] is greater than 8,
     then we must use multiple characters to represent a single array
     entry. *)

  if k <= 8 then begin

    (* [w] is the number of array entries that we pack in a character. *)

    assert (8 mod k = 0);
    let w = 8 / k in

    (* [n] is the length of the string that we allocate. *)

    let n =
      if m mod w = 0 then
        m / w
      else
        m / w + 1
    in

    let s =
      Bytes.create n
    in

    (* Define a reader for the source array. The reader might run off
       the end if [w] does not divide [m]. *)

    let i = ref 0 in
    let next () =
      let ii = !i in
      if ii = m then
        0 (* ran off the end, pad with zeroes *)
      else
        let v = a.(ii) in
        i := ii + 1;
        v
    in

    (* Fill up the string. *)

    for j = 0 to n - 1 do
      let c = ref 0 in
      for _x = 1 to w do
        c := (!c lsl k) lor next()
      done;
      Bytes.set s j (Char.chr !c)
    done;

    (* Done. *)

    k, Bytes.unsafe_to_string s

  end
  else begin (* k > 8 *)

    (* [w] is the number of characters that we use to encode an array entry. *)

    assert (k mod 8 = 0);
    let w = k / 8 in

    (* [n] is the length of the string that we allocate. *)

    let n =
      m * w
    in

    let s =
      Bytes.create n
    in

    (* Fill up the string. *)

    for i = 0 to m - 1 do
      let v = ref a.(i) in
      for x = 1 to w do
        Bytes.set s ((i + 1) * w - x) (Char.chr (!v land 255));
        v := !v lsr 8
      done
    done;

    (* Done. *)

    k, Bytes.unsafe_to_string s

  end

(* Access to a string. *)

let read (s : string) (i : int) : int =
  Char.code (String.unsafe_get s i)

(* [get1 t i] returns the integer stored in the packed array [t] at index [i].
   It assumes (and does not check) that the array's bit width is [1]. The
   parameter [t] is just a string. *)

let get1 (s : string) (i : int) : int =
  let c = read s (i lsr 3) in
  let c = c lsr ((lnot i) land 0b111) in
  let c = c land 0b1 in
  c

(* [get t i] returns the integer stored in the packed array [t] at index [i]. *)

(* Together, [pack] and [get] satisfy the following property: if the index [i]
   is within bounds, then [get (pack a) i] equals [a.(i)]. *)

let get ((k, s) : t) (i : int) : int =
  match k with
  | 1 ->
      get1 s i
  | 2 ->
      let c = read s (i lsr 2) in
      let c = c lsr (2 * ((lnot i) land 0b11)) in
      let c = c land 0b11 in
      c
  | 4 ->
      let c = read s (i lsr 1) in
      let c = c lsr (4 * ((lnot i) land 0b1)) in
      let c = c land 0b1111 in
      c
  | 8 ->
      read s i
  | 16 ->
      let j = 2 * i in
      (read s j) lsl 8 + read s (j + 1)
  | _ ->
      assert (k = 32); (* 64 bits unlikely, not supported *)
      let j = 4 * i in
      (((read s j lsl 8) + read s (j + 1)) lsl 8 + read s (j + 2)) lsl 8 + read s (j + 3)

(* [unflatten1 (n, data) i j] accesses the two-dimensional bitmap
   represented by [(n, data)] at indices [i] and [j]. The integer
   [n] is the width of the bitmap; the string [data] is the second
   component of the packed array obtained by encoding the table as
   a one-dimensional array. *)

let unflatten1 (n, data) i j =
   get1 data (n * i + j)

