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

type raw_symbol =
  string * Lexing.position * Lexing.position

type raw_nonterminal =
  raw_symbol

type raw_terminal =
  raw_symbol

type raw_sentence =
  raw_nonterminal option * raw_terminal list

type located_raw_sentence =
  Positions.positions * raw_sentence

type sentence =
  Nonterminal.t option * Terminal.t list

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

let or_comment_fold f accu = function
  | Thing s ->
      f accu s
  | Comment _ ->
      accu

let or_comment_map f = function
  | Thing s ->
      Thing (f s)
  | Comment c ->
      Comment c

let or_comment_filter_map f = function
  | Thing s ->
      Some (f s)
  | Comment _ ->
      None

let or_comment_count accu = function
  | Thing _ ->
      accu + 1
  | Comment _ ->
      accu

let count_things (xs : 'a or_comment list) =
  List.fold_left or_comment_count 0 xs
