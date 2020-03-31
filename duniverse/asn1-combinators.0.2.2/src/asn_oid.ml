(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(* XXX
 * OIDs being just ints means not being able to represent the full range.
 * Rarely used in practice, but maybe switch to bignums.
 *)
type t = Oid of int * int * int list

let invalid_arg fmt = Format.ksprintf invalid_arg fmt

let (<|) (Oid (v1, v2, vs)) vn =
  if vn < 0 then invalid_arg "OID.(<|): negative component: %d" vn;
  Oid (v1, v2, vs @ [vn])

let (<||) (Oid (v1, v2, vs)) vs' =
  let f v = if v < 0 then invalid_arg "OID.(<||): negative component: %d" v in
  List.iter f vs;
  Oid (v1, v2, vs @ vs')

let base v1 v2 =
  match v1 with
  | 0|1 when v2 >= 0 && v2 < 40 -> Oid (v1, v2, [])
  | 2   when v2 >= 0            -> Oid (v1, v2, [])
  | _ -> invalid_arg "OID.base: out of range: %d.%d" v1 v2

let base_opt v1 v2 = try Some (base v1 v2) with Invalid_argument _ -> None

let to_nodes (Oid (v1, v2, vs)) = (v1, v2, vs)

let of_nodes n1 n2 ns =
  try Some (base n1 n2 <|| ns) with Invalid_argument _ -> None

let pp ppf (Oid (v1, v2, vs)) =
  Format.fprintf ppf "%d.%d%a" v1 v2
  (fun ppf -> List.iter (Format.fprintf ppf ".%d")) vs

let of_string s =
  let rec go ic =
    if Scanf.Scanning.end_of_input ic then [] else
      Scanf.bscanf ic ".%d%r" go (fun n ns -> n :: ns) in
  try Scanf.sscanf s "%d.%d%r" go of_nodes
  with End_of_file | Scanf.Scan_failure _ -> None

let compare (Oid (v1, v2, vs)) (Oid (v1', v2', vs')) =
  let rec cmp (xs: int list) ys = match (xs, ys) with
    | ([], []) ->  0
    | ([], _ ) -> -1
    | (_ , []) ->  1
    | (x::xs, y::ys) -> match compare x y with 0 -> cmp xs ys | r -> r in
  match compare v1 v1' with
  | 0 -> ( match compare v2 v2' with 0 -> cmp vs vs' | r -> r )
  | r -> r

let equal o1 o2 = compare o1 o2 = 0

let seeded_hash seed (Oid (v1, v2, vs)) =
  Hashtbl.(List.fold_left seeded_hash (seeded_hash (seeded_hash seed v1) v2) vs)

let hash o = seeded_hash 0 o
