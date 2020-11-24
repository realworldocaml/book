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

(* This is a pretty-printer for grammars. *)

(* If the [mode] parameter requests ``unit actions'', then semantic actions
   are dropped: that is, they are replaced with trivial semantic actions that
   return unit. Accordingly, all [%type] declarations are changed to unit. The
   prologue and epilogue are dropped. All bindings for semantic values are
   suppressed.

   If, furthermore, the [mode] parameter requests ``unit tokens'', then the
   types carried by tokens are changed to unit. *)

val print: Settings.print_mode -> out_channel -> BasicSyntax.grammar -> unit

