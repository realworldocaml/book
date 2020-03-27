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

type 'a t =
| Reachable of 'a Seq.seq
| Unreachable

let equal p1 p2 =
  match p1, p2 with
  | Reachable _, Reachable _
  | Unreachable, Unreachable ->
      true
  | _, _ ->
      false

let bottom =
  Unreachable

let epsilon =
  Reachable Seq.empty

let singleton x =
  Reachable (Seq.singleton x)

let is_maximal p =
  match p with
  | Reachable _ ->
      true
  | _ ->
      false

let min p1 p2 =
  match p1, p2 with
  | Reachable _, _ ->
       p1
  | Unreachable, p ->
      p

let min_lazy p1 p2 =
  match p1 with
  | Reachable _ ->
      p1
  | Unreachable ->
      p2()

let add p1 p2 =
  match p1, p2 with
  | Reachable xs1, Reachable xs2 ->
      Reachable (Seq.append xs1 xs2)
  | _, _ ->
      Unreachable

let add_lazy p1 p2 =
  match p1 with
  | Unreachable ->
      Unreachable
  | Reachable _ ->
      add p1 (p2())

let print conv p =
  match p with
  | Reachable xs ->
      String.concat " " (List.map conv (Seq.elements xs))
  | Unreachable ->
      "unreachable"
