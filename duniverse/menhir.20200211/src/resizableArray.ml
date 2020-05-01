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

(* This module implements resizable arrays, that is, arrays that can
   grow upon explicit request. *)

type 'a t = {
    (* The default element is used to fill empty slots when growing or shrinking
       the physical array. *)
    default: 'a;
    (* The init function is used to initialize newly allocated slots when
       growing the logical array. *)
    init: int -> 'a;
    (* The logical size of this array. *)
    mutable size: int;
    (* The physical array, whose length is at least [size]. *)
    mutable table: 'a array
  }

let make capacity default init =
  (* [capacity] must be nonzero, so that doubling it actually
     enlarges the array. *)
  assert (capacity >= 0);
  let capacity = if capacity = 0 then 1 else capacity in
  let table = Array.make capacity default in
  { default; init; size = 0; table }

let make_ capacity default =
  make capacity default (fun _ -> default)

let length a =
  a.size

let get a i =
  assert (0 <= i && i < a.size);
  Array.unsafe_get a.table (i)

let set a i x =
  assert (0 <= i && i < a.size);
  Array.unsafe_set a.table (i) x

let shrink a s =
  (* This is [resize a s], assuming [0 <= s < a.size]. *)
  Array.fill a.table s (a.size - s) a.default;
  a.size <- s

let grow a s =
  (* This is [resize a s], assuming [0 <= s && a.size < s]. *)
  let n = Array.length a.table in
  if s > n then begin
    (* The physical size of the array must increase. The new size is at
       least double of the previous size, and larger if requested. *)
    let table = Array.make (max (2 * n) s) a.default in
    Array.blit a.table 0 table 0 n;
    a.table <- table
  end;
  (* From [a.size] to [s], we have new logical slots. Initialize them. *)
  let init = a.init
  and table = a.table in
  for i = a.size to s - 1 do
    Array.unsafe_set table i (init i)
  done;
  (* Update the array's logical size. *)
  a.size <- s

let resize a s =
  assert (0 <= s);
  if s < a.size then
    shrink a s
  else if s > a.size then
    grow a s

let push a x =
  let s = a.size in                       (* equivalent to: [length a] *)
  begin                                   (* equivalent to: [resize a (s + 1)] *)
    let s = s + 1 in
    let n = Array.length a.table in
    if s > n then begin
      (* assert (s = n + 1); *)
      (* assert (max (2 * n) s = 2 * n); *)
      let table = Array.make (2 * n) a.default in
      Array.blit a.table 0 table 0 n;
      a.table <- table
    end;
    (* No need to call [init], since there is just one new logical slot
       and we are about to write it anyway. *)
    a.size <- s
  end;
  Array.unsafe_set a.table (s) x          (* equivalent to: [set a s x] *)

let pop a =
  let s = a.size in                       (* equivalent to: [length a] *)
  assert (s > 0);
  let s = s - 1 in
  a.size <- s;
  let table = a.table in
  let x = Array.unsafe_get table (s) in (* equivalent to: [get a s] *)
  Array.unsafe_set table (s) a.default; (* equivalent to: [resize a s] *)
  x

let default a =
  a.default

