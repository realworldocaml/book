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

(* Hash tables *)

external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" "noalloc"

let hash x = seeded_hash_param 10 100 0 x

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of entries *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
  }

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

(* Creating a fresh, empty table *)

let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let create initial_size =
  let s = power_2_above 16 initial_size in
  { size = 0; data = Array.make s Empty }

(* careful: [key_index] has been manually inlined several times below *)
let key_index h key =
  (hash key) land (Array.length h.data - 1)

let rec insert_bucket ndata mask = function
    Empty -> ()
  | Cons(key, data, rest) ->
      insert_bucket ndata mask rest; (* preserve original order of elements *)
      let nidx = (hash key) land mask in
      ndata.(nidx) <- Cons(key, data, ndata.(nidx))

let resize h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    h.data <- ndata;
    let mask = Array.length ndata - 1 in
    for i = 0 to osize - 1 do
      insert_bucket ndata mask odata.(i)
    done
  end

let rec find_rec key = function
  | Empty ->
      raise Not_found
  | Cons(k, d, rest) ->
      if key = k then d else find_rec key rest

let find h key =
  let data = h.data in
  let i = (hash key) land (Array.length data - 1) in
  match data.(i) with
  | Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
      if key = k1 then d1 else
      match rest1 with
      | Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
          if key = k2 then d2 else
          match rest2 with
          | Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
              if key = k3 then d3 else find_rec key rest3

let rec mem_in_bucket key = function
| Empty ->
    false
| Cons(k, _, rest) ->
    k = key || mem_in_bucket key rest

let mem h key =
  mem_in_bucket key h.data.(key_index h key)

let add h key info =
  let i = key_index h key in
  let bucket = Cons(key, info, h.data.(i)) in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize h

(* [mem_add] combines [mem] and [add] if absent *)
let mem_add h key info =
  let data = h.data in
  let width = Array.length data in
  let i = (hash key) land (width - 1) in
  let bucket = data.(i) in
  if mem_in_bucket key bucket then
    false
  else begin
    data.(i) <- Cons(key, info, bucket);
    h.size <- h.size + 1;
    if h.size > width lsl 1 then resize h;
    true
  end

