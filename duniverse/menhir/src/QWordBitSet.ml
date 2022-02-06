(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module offers bitsets that fit within *four* OCaml integers. This can
   be used to represent sets of integers in the semi-open interval [0, bound),
   where [bound] is [4 * AtomicBitSet.bound], that is, usually 252. *)

(* In principle, we could and should implement this as a pair of two sets of
   type [DWordBitSet.t]. However, we wish to avoid nesting heap-allocated
   pairs, so we have to manually adapt the code in [DWordBitSet]. *)

module A =
  AtomicBitSet

type t =
  | E
  | Q of A.t * A.t * A.t * A.t

let construct hhi hlo lhi llo =
  if A.is_empty hhi && A.is_empty hlo && A.is_empty lhi && A.is_empty llo then
    E
  else
    Q (hhi, hlo, lhi, llo)

type element =
  int

let bound =
  4 * A.bound

let quarter3 =
  3 * A.bound

let middle =
  2 * A.bound

let quarter =
  1 * A.bound

let empty =
  E

let is_empty s =
  match s with
  | E ->
      true
  | Q (_, _, _, _) ->
      false

let singleton i =
  if i < middle then
    if i < quarter then
      Q (A.empty, A.empty, A.empty, A.singleton i)
    else
      let i = i - quarter in
      Q (A.empty, A.empty, A.singleton i, A.empty)
  else
    let i = i - middle in
    if i < quarter then
      Q (A.empty, A.singleton i, A.empty, A.empty)
    else
      let i = i - quarter in
      Q (A.singleton i, A.empty, A.empty, A.empty)

let add i s =
  match s with
  | E ->
      singleton i
  | Q (hhi, hlo, lhi, llo) ->
      if i < middle then
        if i < quarter then
          let llo' = A.add i llo in
          if llo == llo' then s else Q (hhi, hlo, lhi, llo')
        else
          let i = i - quarter in
          let lhi' = A.add i lhi in
          if lhi == lhi' then s else Q (hhi, hlo, lhi', llo)
      else
        let i = i - middle in
        if i < quarter then
          let hlo' = A.add i hlo in
          if hlo == hlo' then s else Q (hhi, hlo', lhi, llo)
        else
          let i = i - quarter in
          let hhi' = A.add i hhi in
          if hhi == hhi' then s else Q (hhi', hlo, lhi, llo)

let remove i s =
  match s with
  | E ->
      s
  | Q (hhi, hlo, lhi, llo) ->
      if i < middle then
        if i < quarter then
          let llo' = A.remove i llo in
          if llo == llo' then s else construct hhi hlo lhi llo'
        else
          let i = i - quarter in
          let lhi' = A.remove i lhi in
          if lhi == lhi' then s else construct hhi hlo lhi' llo
      else
        let i = i - middle in
        if i < quarter then
          let hlo' = A.remove i hlo in
          if hlo == hlo' then s else construct hhi hlo' lhi llo
        else
          let i = i - quarter in
          let hhi' = A.remove i hhi in
          if hhi == hhi' then s else construct hhi' hlo lhi llo

let fold f s accu =
  match s with
  | E ->
      accu
  | Q (hhi, hlo, lhi, llo) ->
      let accu = A.fold f llo accu in
      let accu = A.fold_delta quarter f lhi accu in
      let accu = A.fold_delta middle f hlo accu in
      let accu = A.fold_delta quarter3 f hhi accu in
      accu

let iter f s =
  match s with
  | E ->
      ()
  | Q (hhi, hlo, lhi, llo) ->
      A.iter f llo;
      A.iter_delta quarter f lhi;
      A.iter_delta middle f hlo;
      A.iter_delta quarter3 f hhi

let is_singleton s =
  match s with
  | E ->
      false
  | Q (hhi, hlo, lhi, llo) ->
      A.is_singleton hhi && A.is_empty hlo && A.is_empty lhi && A.is_empty llo ||
      A.is_empty hhi && A.is_singleton hlo && A.is_empty lhi && A.is_empty llo ||
      A.is_empty hhi && A.is_empty hlo && A.is_singleton lhi && A.is_empty llo ||
      A.is_empty hhi && A.is_empty hlo && A.is_empty lhi && A.is_singleton llo

let cardinal s =
  match s with
  | E ->
      0
  | Q (hhi, hlo, lhi, llo) ->
      A.cardinal hhi + A.cardinal hlo + A.cardinal lhi + A.cardinal llo

let elements s =
  fold (fun tl hd -> tl :: hd) s []

let subset s1 s2 =
  match s1, s2 with
  | E, _ ->
      true
  | Q (_, _, _, _), E ->
      false
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
      A.subset hhi1 hhi2 && A.subset hlo1 hlo2 &&
      A.subset lhi1 lhi2 && A.subset llo1 llo2

let mem i s =
  match s with
  | E ->
      false
  | Q (hhi, hlo, lhi, llo) ->
      if i < middle then
        if i < quarter then
          A.mem i llo
        else
          let i = i - quarter in
          A.mem i lhi
      else
        let i = i - middle in
        if i < quarter then
          A.mem i hlo
        else
          let i = i - quarter in
          A.mem i hhi

let union s1 s2 =
  match s1, s2 with
  | E, s
  | s, E ->
      s
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
      let hhi = A.union hhi1 hhi2
      and hlo = A.union hlo1 hlo2
      and lhi = A.union lhi1 lhi2
      and llo = A.union llo1 llo2 in
      if hhi == hhi2 && hlo == hlo2 && lhi == lhi2 && llo == llo2 then s2
      else Q (hhi, hlo, lhi, llo)

let inter s1 s2 =
  match s1, s2 with
  | E, _
  | _, E ->
      E
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
      construct
        (A.inter hhi1 hhi2) (A.inter hlo1 hlo2)
        (A.inter lhi1 lhi2) (A.inter llo1 llo2)

let choose s =
  match s with
  | E ->
      raise Not_found
  | Q (hhi, hlo, lhi, llo) ->
      if not (A.is_empty llo) then
        A.choose llo
      else if not (A.is_empty lhi) then
        A.choose lhi + quarter
      else if not (A.is_empty hlo) then
        A.choose hlo + middle
      else
        A.choose hhi + quarter3

let compare s1 s2 =
  if s1 == s2 then 0
  else match s1, s2 with
  | E  , E   -> 0
  | Q _, E   -> 1
  | E  , Q _ -> -1
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
    begin match A.compare hhi1 hhi2 with
      | 0 ->
        begin match A.compare hlo1 hlo2 with
          | 0 ->
            begin match A.compare lhi1 lhi2 with
              | 0 -> A.compare llo1 llo2
              | n -> n
            end
          | n -> n
        end
      | n -> n
    end

let equal s1 s2 =
  (s1 == s2) ||
  match s1, s2 with
  | E , E -> true
  | Q _, E | E , Q _ -> false
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
    A.equal hhi1 hhi2 &&
    A.equal hlo1 hlo2 &&
    A.equal lhi1 lhi2 &&
    A.equal llo1 llo2

let disjoint s1 s2 =
  match s1, s2 with
  | E, _
  | _, E ->
      true
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
      A.disjoint hhi1 hhi2 && A.disjoint hlo1 hlo2 &&
      A.disjoint lhi1 lhi2 && A.disjoint llo1 llo2

let quick_subset s1 s2 =
  match s1, s2 with
  | E, _ | _, E -> false
  | Q (hh1, hl1, lh1, ll1), Q (hh2, hl2, lh2, ll2) ->
    A.quick_subset ll1 ll2 ||
    A.quick_subset lh1 lh2 ||
    A.quick_subset hl1 hl2 ||
    A.quick_subset hh1 hh2

let compare_minimum s1 s2 =
  match s1, s2 with
  | E, E -> 0
  | E, _ -> -1
  | _, E -> 1
  | Q (hh1, hl1, lh1, ll1), Q (hh2, hl2, lh2, ll2) ->
    match A.is_empty ll1, A.is_empty ll2 with
    | false, false -> A.compare_minimum ll1 ll2
    | true , false -> 1
    | false, true  -> -1
    | true , true  ->
      match A.is_empty lh1, A.is_empty lh2 with
      | false, false -> A.compare_minimum lh1 lh2
      | true , false -> 1
      | false, true  -> -1
      | true , true  ->
        match A.is_empty hl1, A.is_empty hl2 with
        | false, false -> A.compare_minimum hl1 hl2
        | true , false -> 1
        | false, true  -> -1
        | true , true  ->
          A.compare_minimum hh1 hh2

let sorted_union xs =
  List.fold_left union empty xs

let extract_unique_prefix s1 s2 =
  match s1, s2 with
  | E, _ -> E, E
  | _, E -> invalid_arg "extract_unique_prefix: r < l"
  | Q (hh1, hl1, lh1, ll1), Q (hh2, hl2, lh2, ll2) ->
    if not (A.is_empty ll2) then (
      if A.is_empty ll1 then invalid_arg "extract_unique_prefix: r < l";
      let p, r = A.extract_unique_prefix ll1 ll2 in
      construct A.empty A.empty A.empty p,
      construct hh1 hl1 lh1 r
    ) else if not (A.is_empty lh2) then (
      if A.is_empty ll1 && A.is_empty lh1 then
        invalid_arg "extract_unique_prefix: r < l";
      let p, r = A.extract_unique_prefix lh1 lh2 in
      construct A.empty A.empty p ll1,
      construct hh1 hl1 r A.empty
    ) else if not (A.is_empty hl2) then (
      if A.is_empty ll1 && A.is_empty lh1 && A.is_empty hl1 then
        invalid_arg "extract_unique_prefix: r < l";
      let p, r = A.extract_unique_prefix hl1 hl2 in
      construct A.empty p lh1 ll1,
      construct hh1 r A.empty A.empty
    ) else (
      if A.is_empty ll1 && A.is_empty lh1 &&
         A.is_empty hl1 && A.is_empty hh1 then
        invalid_arg "extract_unique_prefix: r < l";
      let p, r = A.extract_unique_prefix hh1 hh2 in
      construct p hl1 lh1 ll1,
      construct r A.empty A.empty A.empty
    )

let extract_shared_prefix s1 s2 =
  match s1, s2 with
  | _, E | E, _ -> E, (s1, s2)
  | Q (hh1, hl1, lh1, ll1), Q (hh2, hl2, lh2, ll2) ->
    if not (A.equal ll1 ll2) then
      let ll, (ll1, ll2) = A.extract_shared_prefix ll1 ll2 in
      construct A.empty A.empty A.empty ll,
      (construct hh1 hl1 lh1 ll1,
       construct hh2 hl2 lh2 ll2)
    else if not (A.equal lh1 lh2) then
      let lh, (lh1, lh2) = A.extract_shared_prefix lh1 lh2 in
      construct A.empty A.empty lh ll1,
      (construct hh1 hl1 lh1 A.empty,
       construct hh2 hl2 lh2 A.empty)
    else if not (A.equal hl1 hl2) then
      let hl, (hl1, hl2) = A.extract_shared_prefix hl1 hl2 in
      construct A.empty hl lh1 ll1,
      (construct hh1 hl1 A.empty A.empty,
       construct hh2 hl2 A.empty A.empty)
    else if not (A.equal hh1 hh2) then
      let hh, (hh1, hh2) = A.extract_shared_prefix hh1 hh2 in
      construct hh hl1 lh1 ll1,
      (construct hh1 A.empty A.empty A.empty,
       construct hh2 A.empty A.empty A.empty)
    else
      s1, (E, E)
