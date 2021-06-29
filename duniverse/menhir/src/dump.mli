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

(* [dump filename] writes a description of the LR(1) automaton to the
   file [filename]. This function can be invoked either before or after
   conflicts have been resolved and extra reductions have been added.
   In both cases, information about the automaton is provided by the
   modules Grammar and Lr1. *)

open Grammar

module Make (Default : sig

  val has_default_reduction:
    Lr1.node -> (Production.index * TerminalSet.t) option

end) : sig

  val dump: string -> unit

end
