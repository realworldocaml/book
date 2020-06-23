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

open Syntax
open GroundSort

(* [infer_grammar g] performs sort inference for the grammar [g],
   rejecting the grammar if it is ill-sorted. It returns a map of
   (terminal and nonterminal) symbols to ground sorts. *)

type sorts = sort StringMap.t

val infer: grammar -> sorts
