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

include Map.Make (String)

let cardinal s =
  fold (fun _ _ x -> x + 1) s 0

let filter pred map =
  fold (fun key value map ->
          if pred key value then
            add key value map
          else
            map) map empty

let restrict domain map =
  filter (fun k _ -> StringSet.mem k domain) map

let domain map =
  fold (fun key _ acu -> StringSet.add key acu) map StringSet.empty

let multiple_add k v m =
  let vs =
    try
      find k m
    with Not_found ->
      []
  in
  add k (v :: vs) m
