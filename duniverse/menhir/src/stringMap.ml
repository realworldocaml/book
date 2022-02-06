(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include Map.Make (String)

let cardinal s =
  fold (fun _ _ x -> x + 1) s 0

let filter p m =
  fold (fun key v m ->
    if p key v then
      add key v m
    else
      m
  ) m empty

let restrict domain m =
  filter (fun k _ -> StringSet.mem k domain) m

let domain m =
  fold (fun key _ acu -> StringSet.add key acu) m StringSet.empty

let multiple_add k v m =
  let vs =
    try
      find k m
    with Not_found ->
      []
  in
  add k (v :: vs) m

let of_list xs =
  List.fold_left (fun m (x, v) -> add x v m) empty xs
