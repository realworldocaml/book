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

(* This module discovers and publishes information about the automaton.

   It determines the shape of the stack when a state is about to be
   entered, when a production is about to be reduced, and when a goto
   transition is about to be taken.

   It also determines which states should be represented (that is,
   need to physically exist on the stack at runtime) and which symbols
   need to keep track of (start or end) positions.

   It also determines which automaton states could have to deal with an
   [error] token.

   The information computed in this module is used in the code back-end,
   in the Coq back-end, and in the automated production of .messages files.
   It is not used in the table back-end. *)

open Grammar

(* ------------------------------------------------------------------------- *)
(* A representation of stack shapes. *)

(* A word is a representation of a stack or stack suffix. *)

type word

(* [fold] folds over a word. At each cell, [f] is applied to the
   accumulator, to a Boolean flag that tells whether the cell holds a
   state, to the set of possible states of the cell, and to the symbol
   associated with the cell. The stack is visited from bottom to top. *)

val fold: ('a -> bool -> Symbol.t -> Lr1.NodeSet.t -> 'a) -> 'a -> word -> 'a

(* [fold_top f accu s] is analogous to [fold], but only folds over the
   top stack cell, if there is one, so that [f] is either not invoked
   at all or invoked just once. *)

val fold_top: (bool -> Symbol.t -> 'a) -> 'a -> word -> 'a

(* [print w] produces a string representation of the word [w]. Only the
   symbols are shown. One space is printed in front of each symbol. *)

val print: word -> string

(* ------------------------------------------------------------------------- *)
(* Information about the stack. *)

(* [stack s] is the structure of the stack at state [s]. *)

val stack: Lr1.node -> word

(* [prodstack prod] is the structure of the stack when production
   [prod] is about to be reduced. This function should not be called
   if production [prod] is never reduced. *)

val prodstack: Production.index -> word

(* [gotostack nt] is the structure of the stack when a shift
   transition over nonterminal [nt] is about to be taken. It
   consists of just one cell. *)

val gotostack: Nonterminal.t -> word

(* [rewind s] explains how to rewind the stack when dealing with an
   error in state [s]. It produces an instruction to either die
   (because no state on the stack can handle errors) or pop a suffix
   of the stack. In the latter case, one reaches a state that is
   either represented (its identity is physically stored in the
   bottommost cell that is popped) or unrepresented (its identity is
   statically known). *)

type instruction =
  | Die
  | DownTo of word * state

and state =
  | Represented
  | UnRepresented of Lr1.node

val rewind: Lr1.node -> instruction

(* ------------------------------------------------------------------------- *)
(* Information about which states and positions need to physically
   exist on the stack. *)

(* [represented s] tells whether state [s] must have an explicit
   representation, that is, whether it is pushed onto the stack. *)

val represented: Lr1.node -> bool

(* [startp symbol] and [endp symbol] tell whether start or end
   positions must be recorded for symbol [symbol]. *)

val startp: Symbol.t -> bool
val endp: Symbol.t -> bool

(* ------------------------------------------------------------------------- *)
(* Information about error handling. *)

(* [errorpeeker s] tells whether state [s] can potentially peek at an
   error. This is the case if, in state [s], an error token may be on
   the stream. *)

val errorpeeker: Lr1.node -> bool

(* ------------------------------------------------------------------------- *)
(* Miscellaneous. *)

(* [universal symbol] tells whether every represented state has an
   outgoing transition along [symbol]. *)

val universal: Symbol.t -> bool
