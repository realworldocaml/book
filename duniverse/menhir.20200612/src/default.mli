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

(* [has_default_reduction s] tells whether state [s] has a default reduction,
   and, if so, upon which set of tokens. *)

val has_default_reduction : Lr1.node -> (Production.index * TerminalSet.t) option

(* [has_reduction s z] tells whether state [s] is willing to reduce some
   production (and if so, which one) when the lookahead symbol is [z]. It
   takes a possible default reduction into account. [z] must be real. *)

val has_reduction: Lr1.node -> Terminal.t -> Production.index option

(* [causes_an_error s z] tells whether state [s] will initiate an error on the
   lookahead symbol [z]. [z] must be real. *)

val causes_an_error: Lr1.node -> Terminal.t -> bool
