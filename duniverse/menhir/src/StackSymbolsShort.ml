(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* An artificial dependency. We must ensure that [Conflict] runs first, so
   that the automaton is in its definitive shape, before we analyze it. *)

module C =
  Conflict

include StackSymbols.Short()
