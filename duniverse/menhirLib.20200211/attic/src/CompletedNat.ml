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

(* The natural numbers, completed with [Infinity], and ordered towards
   zero (i.e. [Infinity] is [bottom], [Finite 0] is [top]). *)

type t =
| Finite of int
| Infinity

type property =
  t

let equal p1 p2 =
  match p1, p2 with
  | Finite i1, Finite i2 ->
      i1 = i2
  | Infinity, Infinity ->
      true
  | _, _ ->
      false

let bottom =
  Infinity

let epsilon =
  Finite 0

let singleton _ =
  Finite 1

let is_maximal p =
  match p with
  | Finite 0 ->
      true
  | _ ->
      false

let min p1 p2 =
  match p1, p2 with
  | Finite i1, Finite i2 ->
      if i1 <= i2 then p1 else p2
  | p, Infinity
  | Infinity, p ->
      p

let min_lazy p1 p2 =
  match p1 with
  | Finite 0 ->
      p1
  | _ ->
      min p1 (p2())

let add p1 p2 =
  match p1, p2 with
  | Finite i1, Finite i2 ->
      Finite (i1 + i2)
  | _, _ ->
      Infinity

let add_lazy p1 p2 =
  match p1 with
  | Infinity ->
      Infinity
  | _ ->
      add p1 (p2())

let print p =
  match p with
  | Finite i ->
      string_of_int i
  | Infinity ->
      "infinity"
