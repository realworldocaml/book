(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This data structure implements sets of integers (of unbounded magnitude). *)

module A =
  AtomicBitSet

(* A sparse bit set is a linked list pairs of an index and a bit set. The list
   is sorted by order of increasing indices. *)

type t =
  | N
  | C of int * A.t * t

type element =
  int

let empty =
  N

let is_empty = function
  | N ->
      true
  | C _ ->
      false

let rec add base offset s =
  match s with
  | N ->
      (* Insert at end. *)
      C (base, A.singleton offset, N)
  | C (addr, ss, qs) ->
      if base < addr then
        (* Insert in front. *)
        C (base, A.singleton offset, s)
      else if base = addr then
        (* Found appropriate cell, update bit field. *)
        let ss' = A.add offset ss in
        if A.equal ss' ss then s else C (addr, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = add base offset qs in
        if qs == qs' then s else C (addr, ss, qs')

let add i s =
  let offset = i mod A.bound in
  let base = i - offset in
  add base offset s

let singleton i =
  (* This is [add i N], specialised. *)
  let offset = i mod A.bound in
  let base = i - offset in
  C (base, A.singleton offset, N)

let rec remove base offset s =
  match s with
  | N ->
      N
  | C (addr, ss, qs) ->
      if base < addr then
        s
      else if base = addr then
        (* Found appropriate cell, update bit field. *)
        let ss' = A.remove offset ss in
        if A.is_empty ss' then
          qs
        else if A.equal ss' ss then s else C (addr, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = remove base offset qs in
        if qs == qs' then s else C (addr, ss, qs')

let remove i s =
  let offset = i mod A.bound in
  let base = i - offset in
  remove base offset s

let rec mem base offset s =
  match s with
  | N ->
      false
  | C (addr, ss, qs) ->
      if base < addr then
        false
      else if base = addr then
        A.mem offset ss
      else
        mem base offset qs

let mem i s =
  let offset = i mod A.bound in
  let base = i - offset in
  mem base offset s

let rec fold f s accu =
  match s with
  | N ->
      accu
  | C (addr, ss, qs) ->
      let accu = A.fold_delta addr f ss accu in
      fold f qs accu

let rec iter f s =
  match s with
  | N ->
      ()
  | C (addr, ss, qs) ->
      A.iter_delta addr f ss;
      iter f qs

let is_singleton s =
  match s with
  | C (_, ss, N) ->
      A.is_singleton ss
  | C (_, _, C _)
  | N ->
      false

let rec cardinal accu s =
  match s with
  | C (_, ss, qs) ->
      let accu = accu + A.cardinal ss in
      cardinal accu qs
  | N ->
      accu

let cardinal s =
  cardinal 0 s

let elements s =
  fold (fun tl hd -> tl :: hd) s []

let rec subset s1 s2 =
  match s1, s2 with
  | N, _ ->
      true
  | _, N ->
      false
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        false
      else if addr1 = addr2 then
        A.subset ss1 ss2 && subset qs1 qs2
      else
        subset s1 qs2

(* [union] preserves sharing (if possible) between its second argument
   and its result. *)

let rec union s1 s2 =
  match s1, s2 with
  | N, s
  | s, N ->
      s
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        C (addr1, ss1, union qs1 s2)
      else if addr1 > addr2 then
        let s = union s1 qs2 in
        if s == qs2 then s2 else C (addr2, ss2, s)
      else
        let ss = A.union ss1 ss2 in
        let s = union qs1 qs2 in
        if A.equal ss ss2 && s == qs2 then s2 else C (addr1, ss, s)

(* [inter] arbitrarily attempts to preserve sharing between its first
   argument and its result. *)

let rec inter s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
      N
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 < addr2 then
        inter qs1 s2
      else if addr1 > addr2 then
        inter s1 qs2
      else
        let ss = A.inter ss1 ss2 in
        let s = inter qs1 qs2 in
        if A.is_empty ss then
          s
        else
          if A.equal ss ss1 && s == qs1 then s1 else C (addr1, ss, s)

let choose s =
  match s with
  | N ->
      raise Not_found
  | C (addr, ss, _) ->
      assert (not (A.is_empty ss));
      addr + A.choose ss

let rec compare x y =
  if x == y then 0 else
    match x, y with
    | C (a1, ss1, qs1), C (a2, ss2, qs2) ->
      begin match Generic.compare a1 a2 with
        | 0 -> begin match A.compare ss1 ss2 with
            | 0 -> compare qs1 qs2
            | n -> n
          end
        | n -> n
      end
    | N, N -> 0
    | C _, N -> 1
    | N, C _ -> -1

let rec equal x y =
  (x == y) ||
  match x, y with
  | C (a1, ss1, qs1), C (a2, ss2, qs2) ->
    a1 = a2 &&
    A.equal ss1 ss2 &&
    equal qs1 qs2
  | N, N -> true
  | C _, N | N, C _ -> false

let rec disjoint s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
      true
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2) ->
      if addr1 = addr2 then
        A.disjoint ss1 ss2 && disjoint qs1 qs2
      else if addr1 < addr2 then
        disjoint qs1 s2
      else
        disjoint s1 qs2

let rec quick_subset a1 ss1 = function
  | N -> false
  | C (a2, ss2, qs2) ->
    if a1 = a2 then
      AtomicBitSet.quick_subset ss1 ss2
    else
      (a1 > a2 && quick_subset a1 ss1 qs2)

let quick_subset s1 s2 =
  match s1 with
  | N -> true
  | C (a1, ss1, _) ->
    (* We know that, by construction, ss1 is not empty.
       It suffices to test s2 also has elements in common with ss1 at address
       a1 to determine the quick_subset relation. *)
    quick_subset a1 ss1 s2

let compare_minimum s1 s2 =
  match s1, s2 with
  | N, N -> 0
  | N, _ -> -1
  | _, N -> 1
  | C (addr1, ss1, _), C (addr2, ss2, _) ->
    match Int.compare addr1 addr2 with
    | 0 -> AtomicBitSet.compare_minimum ss1 ss2
    | n -> n

let sorted_union xs =
  (* It is important to start folding from the right end of the list.
     Since elements are sorted, by starting from the right end we only prepend
     elements, that makes the algorithm linear in the number of items.
     Starting from the left end would make it quadratic, revisiting the prefix
     of a list that get longer and longer as elements are added. *)
  List.fold_right union xs empty

let rec extract_unique_prefix addr2 ss2 = function
  | N -> N, N
  | C (addr1, ss1, qs1) as self ->
    if addr1 < addr2 then
      let prefix, suffix = extract_unique_prefix addr2 ss2 qs1 in
      C (addr1, ss1, prefix), suffix
    else if addr1 > addr2
         || AtomicBitSet.equal ss1 ss2 then
      N, self
    else
      (* l and r have the same address, and
         l has some prefix that is not part of r (lsb l < lsb r)*)
      let ss0, ss1 = AtomicBitSet.extract_unique_prefix ss1 ss2 in
      if AtomicBitSet.is_empty ss0 then
        N, self
      else if AtomicBitSet.is_empty ss1 then
        (C (addr1, ss0, N), qs1)
      else
        (C (addr1, ss0, N), C (addr1, ss1, qs1))

let extract_unique_prefix l r =
  match l, r with
  | N, _ -> N, N
  | _, N -> invalid_arg "extract_unique_prefix: r < l"
  | l, C (addr2, ss2, _) -> extract_unique_prefix addr2 ss2 l

let rec extract_shared_prefix = function
  | C (addr1, ss1, qs1), C (addr2, ss2, qs2)
    when addr1 = addr2 ->
    if AtomicBitSet.equal ss1 ss2 then
      let common, rest = extract_shared_prefix (qs1, qs2) in
      (C (addr1, ss1, common), rest)
    else
      let common, (ss1, ss2) = AtomicBitSet.extract_shared_prefix ss1 ss2 in
      let common =
        if AtomicBitSet.is_empty common then N else C (addr1, common, N)
      in
      let qs1 =
        if AtomicBitSet.is_empty ss1 then qs1 else C (addr1, ss1, qs1)
      in
      let qs2 =
        if AtomicBitSet.is_empty ss2 then qs2 else C (addr2, ss2, qs2)
      in
      common, (qs1, qs2)
  | (l, r) -> N, (l, r)

let extract_shared_prefix l r = extract_shared_prefix (l, r)
