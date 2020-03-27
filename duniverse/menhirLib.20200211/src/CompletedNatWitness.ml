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
| Finite of int * 'a Seq.seq
| Infinity

let equal p1 p2 =
  match p1, p2 with
  | Finite (i1, _), Finite (i2, _) ->
      i1 = i2
  | Infinity, Infinity ->
      true
  | _, _ ->
      false

let bottom =
  Infinity

let epsilon =
  Finite (0, Seq.empty)

let singleton x =
  Finite (1, Seq.singleton x)

let is_maximal p =
  match p with
  | Finite (0, _) ->
      true
  | _ ->
      false

let min p1 p2 =
  match p1, p2 with
  | Finite (i1, _), Finite (i2, _) ->
      if i1 <= i2 then p1 else p2
  | p, Infinity
  | Infinity, p ->
      p

let min_lazy p1 p2 =
  match p1 with
  | Finite (0, _) ->
      p1
  | _ ->
      min p1 (p2())

let add p1 p2 =
  match p1, p2 with
  | Finite (i1, xs1), Finite (i2, xs2) ->
      Finite (i1 + i2, Seq.append xs1 xs2)
  | _, _ ->
      Infinity

let add_lazy p1 p2 =
  match p1 with
  | Infinity ->
      Infinity
  | _ ->
      add p1 (p2())

let print conv p =
  match p with
  | Finite (i, xs) ->
      Printf.sprintf "(* %d *) " i ^
      String.concat " " (List.map conv (Seq.elements xs))
  | Infinity ->
      "infinity"

let to_int p =
  match p with
  | Finite (i, _) ->
      i
  | Infinity ->
      max_int

let extract p =
  match p with
  | Finite (_, xs) ->
      Seq.elements xs
  | Infinity ->
      assert false
