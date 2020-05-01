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
open SortInference

(* [expand sorts g] expands away some or all of the parameterized nonterminal
   symbols in the grammar [g], producing a new grammar. [sorts] is the sort
   environment produced by [SortInference]. *)

(* The mode [ExpandHigherSort] causes a partial expansion: only the parameters
   of higher sort (i.e., of sort other than [*]) are expanded away. This mode
   is safe, in the sense that expansion always terminates. A proof sketch is
   as follows: 1- an application always has sort [*]; 2- therefore, only a
   variable can have higher sort; 3- therefore, only a finite number of terms
   can appear during expansion. *)

(* The mode [ExpandAll] causes a complete expansion: all parameters are
   expanded away. This process is potentially nonterminating. One must first
   run the termination test in [CheckSafeParameterizedGrammar] (which itself
   is applicable only after the parameters of higher sort have been expanded
   away). *)

type mode =
  | ExpandHigherSort
  | ExpandAll

val expand: mode -> sorts -> grammar -> grammar
