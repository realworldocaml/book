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

(* The [entry] array contains offsets into the [data] array. It has [n+1]
   elements if the original (unencoded) array has [n] elements. The value
   of [entry.(n)] is the length of the [data] array. This convention is
   natural and allows avoiding a special case. *)

type 'a t =
  (* data: *)   'a array *
  (* entry: *) int array

let make (a : 'a array array) : 'a t =
  let n = Array.length a in
  (* Build the entry array. *)
  let size = ref 0 in
  let entry = Array.init (n + 1) (fun i ->
    let s = !size in
    if i < n then
      size := s + Array.length a.(i);
    s
  ) in
  assert (entry.(n) = !size);
  (* Build the data array. *)
  let i = ref 0
  and j = ref 0 in
  let data = Array.init !size (fun _ ->
    while !j = Array.length a.(!i) do
      i := !i + 1;
      j := 0;
    done;
    let x = a.(!i).(!j) in
    j := !j + 1;
    x
  ) in
  data, entry

let length ((_, entry) : 'a t) : int =
  Array.length entry

let row_length ((_, entry) : 'a t) i : int =
  entry.(i + 1) - entry.(i)

let row_length_via get_entry i =
  get_entry (i + 1) - get_entry i

let read ((data, entry) as la : 'a t) i j : 'a =
  assert (0 <= j && j < row_length la i);
  data.(entry.(i) + j)

let read_via get_data get_entry i j =
  assert (0 <= j && j < row_length_via get_entry i);
  get_data (get_entry i + j)

let write ((data, entry) as la : 'a t) i j (v : 'a) : unit =
  assert (0 <= j && j < row_length la i);
  data.(entry.(i) + j) <- v

let rec read_interval_via get_data i j =
  if i = j then
    []
  else
    get_data i :: read_interval_via get_data (i + 1) j

let read_row_via get_data get_entry i =
  read_interval_via get_data (get_entry i) (get_entry (i + 1))

let read_row ((data, entry) : 'a t) i : 'a list =
  read_row_via (Array.get data) (Array.get entry) i

