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

external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" "noalloc"

let hash x =
  seeded_hash_param 10 100 0 x

type 'a t =
  { mutable size: int;
    mutable data: 'a list array;
  }

let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let create initial_size =
  let s = power_2_above 16 initial_size in
  { size = 0; data = Array.make s [] }

(* careful: [key_index] has been manually inlined several times below *)
let key_index h key =
  (hash key) land (Array.length h.data - 1)

let rec insert_bucket ndata mask bucket =
  match bucket with
    [] -> ()
  | key :: rest ->
      let nidx = (hash key) land mask in
      ndata.(nidx) <- key :: ndata.(nidx);
      insert_bucket ndata mask rest

let resize h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize [] in
    h.data <- ndata;
    let mask = Array.length ndata - 1 in
    for i = 0 to osize - 1 do
      insert_bucket ndata mask odata.(i)
    done
  end

let rec find_rec key bucket =
  match bucket with
  | [] ->
      raise Not_found
  | k :: rest ->
      if key = k then k else find_rec key rest

let find h key =
  let data = h.data in
  let i = (hash key) land (Array.length data - 1) in
  find_rec key data.(i)

let add h key =
  let data = h.data in
  let i = (hash key) land (Array.length data - 1) in
  h.data.(i) <- key :: data.(i);
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize h

(* [find_add] combines [find] and [add] if absent *)
let find_add h key =
  let data = h.data in
  let width = Array.length data in
  let i = (hash key) land (width - 1) in
  let bucket = data.(i) in
  try
    find_rec key bucket
  with Not_found ->
    data.(i) <- key :: bucket;
    h.size <- h.size + 1;
    if h.size > width lsl 1 then resize h;
    key

let length h =
  h.size

