(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module IntSet = SparseBitSet

module type DECOMPOSABLE = sig
  type t
  val is_empty : t -> bool
  val compare_minimum : t -> t -> int
  val sorted_union : t list -> t
  val extract_unique_prefix : t -> t -> t * t
  val extract_shared_prefix : t -> t -> t * (t * t)
end

module type S = sig
  type t
  val partition : t list -> t list
  val annotated_partition : (t * 'a) list -> (t * 'a list) list
  val partition_and_total : t list -> t list * t
end

module Make (Set : DECOMPOSABLE) : S with type t := Set.t = struct
  type leftist =
    | Leaf
    | Node of leftist * Set.t * IntSet.t * leftist * int

  let singleton k v = Node (Leaf, k, v, Leaf, 1)
  let rank = function Leaf -> 0 | Node (_,_,_,_,r) -> r

  let rec merge t1 t2 = match t1,t2 with
    | Leaf, t | t, Leaf -> t
    | Node (l, k1, v1, r, _), Node (_, k2, _, _, _) ->
      if Set.compare_minimum k1 k2 > 0 then
        merge t2 t1 (* switch merge if necessary *)
      else
        let merged = merge r t2 in (* always merge with right *)
        let rank_left = rank l and rank_right = rank merged in
        if rank_left >= rank_right
        then Node (l, k1, v1, merged, rank_right+1)
        else Node (merged, k1, v1, l, rank_left+1) (* left becomes right due to being shorter *)

  let heap_insert k v t = merge (singleton k v) t

  type pop =
    | Head of Set.t * IntSet.t * Set.t * IntSet.t * leftist
    | Tail of Set.t * IntSet.t
    | Done

  let heap_pop2 = function
    | Leaf -> Done
    | Node (Leaf, k, v, _, _) ->
      Tail (k, v)
    | Node (Node (ll, lk, lv, lr, _), k, v, Leaf, _) ->
      Head (k, v, lk, lv, merge ll lr)
    | Node (
        (Node (ll, lk, lv, lr, _) as l),
        k, v,
        (Node (rl, rk, rv, rr, _) as r),
        _
      ) ->
      if Set.compare_minimum lk rk <= 0
      then Head (k, v, lk, lv, merge (merge ll lr) r)
      else Head (k, v, rk, rv, merge (merge rl rr) l)

  let compute_parts xs =
    let heap, _ = List.fold_left
        (fun (h, i) s -> heap_insert s (IntSet.singleton i) h, i + 1)
        (Leaf, 0) xs
    in
    let rec aux parts heap =
      match heap_pop2 heap with
      | Head (s1, k1, s2, k2, heap) ->
        let sp, s1 = Set.extract_unique_prefix s1 s2 in
        let sc, (s1, s2) = Set.extract_shared_prefix s1 s2 in
        let parts =
          if not (Set.is_empty sp) then (sp, k1) :: parts else parts
        in
        let heap =
          if not (Set.is_empty sc)
          then heap_insert sc (IntSet.union k1 k2) heap
          else heap
        in
        let heap =
          if not (Set.is_empty s1) then heap_insert s1 k1 heap else heap
        in
        let heap =
          if not (Set.is_empty s2) then heap_insert s2 k2 heap else heap
        in
        aux parts heap
      | Tail (k, v) -> (k, v) :: parts
      | Done -> parts
    in
    aux [] heap

  let union_parts parts =
    match List.sort (fun (_, k1) (_, k2) -> IntSet.compare k1 k2) parts with
    | [] -> []
    | (s1, k1) :: rest ->
      let rec merge acc ss key = function
        | [] -> Set.sorted_union ss :: acc
        | (s, key') :: rest ->
          if IntSet.equal key key' then
            merge acc (s :: ss) key rest
          else
            merge (Set.sorted_union ss :: acc) [s] key' rest
      in
      merge [] [s1] k1 rest

  let partition xs = union_parts (compute_parts xs)

  let partition_and_total xs =
    let parts = compute_parts xs in
    let total = Set.sorted_union (List.rev_map fst parts) in
    let union = union_parts parts in
    union, total

  let keyed_union_parts parts =
    match List.sort (fun (_, k1) (_, k2) -> IntSet.compare k1 k2) parts with
    | [] -> []
    | (s1, k1) :: rest ->
      let rec merge acc ss key = function
        | [] -> (Set.sorted_union ss, key) :: acc
        | (s, key') :: rest ->
          if IntSet.equal key key' then
            merge acc (s :: ss) key rest
          else
            merge ((Set.sorted_union ss, key) :: acc) [s] key' rest
      in
      merge [] [s1] k1 rest

  let annotated_partition xs =
    let sets, annotations = List.split xs in
    let annotations = Array.of_list annotations in
    let parts = compute_parts sets in
    let union = keyed_union_parts parts in
    let annotate key =
      IntSet.fold (fun i acc -> annotations.(i) :: acc) key []
    in
    List.map (fun (set, key) -> set, annotate key) union
end
