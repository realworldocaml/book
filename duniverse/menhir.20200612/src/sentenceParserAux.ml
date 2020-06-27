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

open Grammar

type terminals =
  Terminal.t list

type sentence =
  Nonterminal.t option * terminals

type located_sentence =
  Positions.positions * sentence

type comment =
  string

type 'a or_comment =
| Thing of 'a
| Comment of comment

let or_comment_iter f = function
  | Thing s ->
      f s
  | Comment _ ->
      ()

let or_comment_map f = function
  | Thing s ->
      Thing (f s)
  | Comment c ->
      Comment c

let unThing = function
  | Thing x ->
      [ x ]
  | Comment _ ->
      []
