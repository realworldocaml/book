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

(* This module constructs an LALR automaton for the grammar described by the
   module [Grammar]. *)

(* In this construction, precedence declarations are not taken into account.
   Thus, conflicts are not resolved; no transitions or reductions are removed
   in order to resolve conflicts. As a result, every node is reachable from
   some entry node. *)

open LR1Sigs

module Run () : LR1_AUTOMATON
