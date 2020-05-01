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

(* This module extends the LR(0) automaton with lookahead information in order
   to construct an SLR(1) automaton. The lookahead information is obtained by
   considering the FOLLOW sets. *)

(* This construction is not used by Menhir, but can be used to check whether
   the grammar is in the class SLR(1). This check is performed when the log
   level [lg] is at least 1. *)

val check: unit -> unit
