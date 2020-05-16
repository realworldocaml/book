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

(* This test accepts a parameterized grammar, with the restriction that all
   parameters must have sort [*]. Parameters of higher sort must be eliminated
   prior to running this test: see [SelectiveExpansion]. *)

(* This test succeeds if and only if the expansion of this grammar is safe,
   that is, terminates. *)

val check: Syntax.grammar -> unit
