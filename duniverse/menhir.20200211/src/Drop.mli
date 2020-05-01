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

(* This function translates a grammar from the [Syntax] format
   to the [BasicSyntax] format. Naturally, the grammar
   must not have any parameterized symbols, since these are not
   allowed by the latter format. *)

val drop: Syntax.grammar -> BasicSyntax.grammar
