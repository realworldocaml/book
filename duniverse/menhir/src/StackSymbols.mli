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

(* This module computes the known prefix of the stack, a sequence of symbols,
   in each of the automaton's states. *)

open Grammar

module Run () : sig

  (* [stack_symbols s] is the known prefix of the stack at state [s]. It
     is represented as an array of symbols. By convention, the top of
     the stack is the end of the array. *)

  val stack_symbols: Lr1.node -> Symbol.t array

  (* [print_stack_symbols s] is a printed representation of the known
     prefix of the stack at state [s]. Every symbol is preceded with
     a space. *)

  val print_stack_symbols: Lr1.node -> string

end
