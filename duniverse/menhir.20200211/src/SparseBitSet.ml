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

(* [union] arbitrarily attempts to preserve sharing between its second
   argument and its result. *)

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

let compare =
  (* We violate the [AtomicBitSet] abstraction a tiny little bit and use
     OCaml's generic comparison. We can do so because our representation
     is canonical. *)
  compare (* this is [Generic.compare] *)

let equal =
  (* We violate the [AtomicBitSet] abstraction a tiny little bit and use
     OCaml's generic equality. We can do so because our representation
     is canonical. *)
  (=)

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
