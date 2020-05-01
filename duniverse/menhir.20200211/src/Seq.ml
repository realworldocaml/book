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

(* Sequences with constant time concatenation and linear-time conversion
   to an ordinary list. *)

(* We maintain the invariant that the left-hand side of [SConcat] is never
   an empty sequence. This allows a slight improvement in [first]. *)

type 'a seq =
| SZero
| SOne of 'a
| SConcat of 'a seq * 'a seq

let empty =
  SZero

let singleton x =
  SOne x

let append xs ys =
  match xs with
  | SZero ->
      ys
  | SOne _
  | SConcat _ ->
      SConcat (xs, ys)

let rec elements xs accu =
  match xs with
  | SZero ->
      accu
  | SOne x ->
      x :: accu
  | SConcat (xs1, xs2) ->
      elements xs1 (elements xs2 accu)

let elements xs =
  elements xs []

let rec concat xss =
  match xss with
  | [] ->
      empty
  | xs :: xss ->
      append xs (concat xss)

let rec first xs =
  match xs with
  | SZero ->
      (* We disallow applying [first] to an empty sequence. *)
      assert false
  | SOne x ->
      x
  | SConcat (xs1, _) ->
      (* Our invariant guarantees [xs1] is nonempty. *)
      first xs1

