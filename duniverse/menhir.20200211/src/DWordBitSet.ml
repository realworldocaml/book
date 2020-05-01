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
        D (hi, A.add i lo)
      else
        D (A.add (i - A.bound) hi, lo)

let remove i s =
  match s with
  | E ->
      s
  | D (hi, lo) ->
      if i < A.bound then
        construct hi (A.remove i lo)
      else
        construct (A.remove (i - A.bound) hi) lo

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
      D (A.union hi1 hi2, A.union lo1 lo2)

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

let compare =
  compare (* this is [Generic.compare] *)

let equal s1 s2 =
  s1 = s2

let disjoint s1 s2 =
  match s1, s2 with
  | E, _
  | _, E ->
      true
  | D (hi1, lo1), D (hi2, lo2) ->
      A.disjoint hi1 hi2 && A.disjoint lo1 lo2
