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
          Q (hhi, hlo, lhi, A.add i llo)
        else
          let i = i - quarter in
          Q (hhi, hlo, A.add i lhi, llo)
      else
        let i = i - middle in
        if i < quarter then
          Q (hhi, A.add i hlo, lhi, llo)
        else
          let i = i - quarter in
          Q (A.add i hhi, hlo, lhi, llo)

let remove i s =
  match s with
  | E ->
      s
  | Q (hhi, hlo, lhi, llo) ->
      if i < middle then
        if i < quarter then
          construct hhi hlo lhi (A.remove i llo)
        else
          let i = i - quarter in
          construct hhi hlo (A.remove i lhi) llo
      else
        let i = i - middle in
        if i < quarter then
          construct hhi (A.remove i hlo) lhi llo
        else
          let i = i - quarter in
          construct (A.remove i hhi) hlo lhi llo

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
      Q (A.union hhi1 hhi2, A.union hlo1 hlo2,
         A.union lhi1 lhi2, A.union llo1 llo2)

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

let compare =
  compare (* this is [Generic.compare] *)

let equal s1 s2 =
  s1 = s2

let disjoint s1 s2 =
  match s1, s2 with
  | E, _
  | _, E ->
      true
  | Q (hhi1, hlo1, lhi1, llo1), Q (hhi2, hlo2, lhi2, llo2) ->
      A.disjoint hhi1 hhi2 && A.disjoint hlo1 hlo2 &&
      A.disjoint lhi1 lhi2 && A.disjoint llo1 llo2
