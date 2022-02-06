(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module offers bitsets that fit within *two* OCaml integers. This can
   be used to represent sets of integers in the semi-open interval [0, bound),
   where [bound] is [2 * AtomicBitSet.bound], that is, usually 126. *)

module A =
  AtomicBitSet

(* As a special case, the empty set is represented by the data constructor [E].
   Thus, the empty set requires no memory allocation. In order to maintain a
   unique representation of sets, we forbid the value [D (A.empty, A.empty)].
   The smart constructor [construct] enforces this restriction. *)

type t =
  | E
  | D of A.t * A.t

let construct hi lo =
  if A.is_empty hi && A.is_empty lo then
    E
  else
    D (hi, lo)

type element =
  int

let bound =
  2 * A.bound

let empty =
  E

let is_empty s =
  match s with
  | E ->
      true
  | D (_, _) ->
      (* Assuming every set is built by [construct] above,
         a set whose constructor is [D] cannot be empty. *)
      false

let singleton i =
  if i < A.bound then
    D (A.empty, A.singleton i)
  else
    D (A.singleton (i - A.bound), A.empty)

let add i s =
  match s with
  | E ->
      singleton i
  | D (hi, lo) ->
      if i < A.bound then
        let lo' = A.add i lo in
        if lo == lo' then s else D (hi, lo')
      else
        let hi' = A.add (i - A.bound) hi in
        if hi == hi' then s else D (hi', lo)

let remove i s =
  match s with
  | E ->
      s
  | D (hi, lo) ->
      if i < A.bound then
        let lo' = A.remove i lo in
        if lo == lo' then s else construct hi lo'
      else
        let hi' = A.remove (i - A.bound) hi in
        if hi == hi' then s else construct hi' lo

let fold f s accu =
  match s with
  | E ->
      accu
  | D (hi, lo) ->
      let accu = A.fold f lo accu in
      let accu = A.fold_delta A.bound f hi accu in
      accu

let iter f s =
  match s with
  | E ->
      ()
  | D (hi, lo) ->
      A.iter f lo;
      A.iter_delta A.bound f hi

let is_singleton s =
  match s with
  | E ->
      false
  | D (hi, lo) ->
      A.is_singleton hi && A.is_empty lo ||
      A.is_empty hi && A.is_singleton lo

let cardinal s =
  match s with
  | E ->
      0
  | D (hi, lo) ->
      A.cardinal hi + A.cardinal lo

let elements s =
  (* Note: the list is produced in decreasing order. *)
  fold (fun tl hd -> tl :: hd) s []

let subset s1 s2 =
  match s1, s2 with
  | E, _ ->
      true
  | D (_, _), E ->
      (* Assuming every set is built by [construct] above,
         a set whose constructor is [D] cannot be empty. *)
      false
  | D (hi1, lo1), D (hi2, lo2) ->
      A.subset hi1 hi2 && A.subset lo1 lo2

let mem i s =
  match s with
  | E ->
      false
  | D (hi, lo) ->
      if i < A.bound then
        A.mem i lo
      else
        A.mem (i - A.bound) hi

let union s1 s2 =
  match s1, s2 with
  | E, s
  | s, E ->
      s
  | D (hi1, lo1), D (hi2, lo2) ->
      let hi = A.union hi1 hi2
      and lo = A.union lo1 lo2 in
      if hi == hi2 && lo == lo2 then s2 else D (hi, lo)

let inter s1 s2 =
  match s1, s2 with
  | E, _
  | _, E ->
      E
  | D (hi1, lo1), D (hi2, lo2) ->
      construct (A.inter hi1 hi2) (A.inter lo1 lo2)

let choose s =
  match s with
  | E ->
      raise Not_found
  | D (hi, lo) ->
      if A.is_empty lo then
        A.choose hi + A.bound
      else
        A.choose lo

let compare s1 s2 =
  if s1 == s2 then 0
  else match s1, s2 with
  | E  , E   -> 0
  | D _, E   -> 1
  | E  , D _ -> -1
  | D (hi1, lo1), D (hi2, lo2) ->
    begin match A.compare hi1 hi2 with
      | 0 -> A.compare lo1 lo2
      | n -> n
    end

let equal s1 s2 =
  (s1 == s2) ||
  match s1, s2 with
  | E , E -> true
  | D _, E | E , D _ -> false
  | D (hi1, lo1), D (hi2, lo2) ->
    A.equal hi1 hi2 &&
    A.equal lo1 lo2

let disjoint s1 s2 =
  match s1, s2 with
  | E, _
  | _, E ->
      true
  | D (hi1, lo1), D (hi2, lo2) ->
      A.disjoint hi1 hi2 && A.disjoint lo1 lo2

let quick_subset s1 s2 =
  match s1, s2 with
  | E, _ | _, E -> false
  | D (hi1, lo1), D (hi2, lo2) ->
    A.quick_subset lo1 lo2 ||
    A.quick_subset hi1 hi2

let compare_minimum s1 s2 =
  match s1, s2 with
  | E, E -> 0
  | E, _ -> -1
  | _, E -> 1
  | D (hi1, lo1), D (hi2, lo2) ->
    match A.is_empty lo1, A.is_empty lo2 with
    | true , true  -> A.compare_minimum hi1 hi2
    | false, false -> A.compare_minimum lo1 lo2
    | true , false -> 1
    | false, true  -> -1

let sorted_union xs =
  List.fold_left union empty xs

let extract_unique_prefix s1 s2 =
  match s1, s2 with
  | E, _ -> E, E
  | _, E -> invalid_arg "extract_unique_prefix: r < l"
  | D (hi1, lo1), D (hi2, lo2) ->
    if A.is_empty lo2 then
      let p, r = A.extract_unique_prefix hi1 hi2 in
      (construct p lo1, construct r A.empty)
    else if A.is_empty lo1 then
      invalid_arg "extract_unique_prefix: r < l"
    else
      let p, r = A.extract_unique_prefix lo1 lo2 in
      (construct A.empty p, construct hi1 r)

let extract_shared_prefix s1 s2 =
  match s1, s2 with
  | _, E | E, _ -> E, (s1, s2)
  | D (hi1, lo1), D (hi2, lo2) ->
    if A.equal lo1 lo2 then
      let hi, (hi1, hi2) = A.extract_shared_prefix hi1 hi2 in
      (construct hi lo1, (construct hi1 A.empty, construct hi2 A.empty))
    else
      let lo, (lo1, lo2) = A.extract_shared_prefix lo1 lo2 in
      (construct A.empty lo, (construct hi1 lo1, construct hi2 lo2))
