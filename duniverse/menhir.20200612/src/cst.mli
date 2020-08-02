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

(* Concrete syntax trees. *)

(* A concrete syntax tree is one of a leaf -- which corresponds to a
   terminal symbol; a node -- which corresponds to a non-terminal
   symbol, and whose immediate descendants form an expansion of that
   symbol; or an error leaf -- which corresponds to a point where the
   [error] pseudo-token was shifted. *)

type cst =
  | CstTerminal of Terminal.t
  | CstNonTerminal of Production.index * cst array
  | CstError

(* This is a (mostly) unambiguous printer for concrete syntax trees,
   in an sexp-like notation. *)

val print: out_channel -> cst -> unit

(* This is a pretty-printer for concrete syntax trees. The notation is
   the same as that used by the above printer; the only difference is
   that the [Pprint] library is used to manage indentation. *)

val show: out_channel -> cst -> unit

