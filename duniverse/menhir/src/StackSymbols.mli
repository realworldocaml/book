(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar

module type STACK_SYMBOLS = sig

  (**A property is a description of the known suffix of the stack at state
     [s]. It is represented as an array of symbols. By convention, the top
     of the stack is the end of the array. *)
  type property =
    Symbol.t array

  (**[stack_symbols s] is the known suffix of the stack at state [s]. It
     is represented as an array of symbols. By convention, the top of
     the stack is the end of the array. *)
  val stack_symbols: Lr1.node -> property

  (**[stack_height s] is [Array.length (stack_symbols s)]. *)
  val stack_height: Lr1.node -> int

  (**[production_symbols prod] is the known suffix of the stack at a state
     where production [prod] can be reduced. In the short invariant, the
     length of this suffix is [Production.length prod]. In the long
     invariant, its length can be greater. *)
  val production_symbols: Production.index -> property

  (**[production_height prod] is [Array.length (production_symbols prod)]. *)
  val production_height: Production.index -> int

  (**[goto_symbols nt] is the known suffix of the stack at a state where an
     edge labeled [nt] has just been followed. In the short invariant, the
     length of this suffix is [1]. In the long invariant, its length can be
     greater. *)
  val goto_symbols: Nonterminal.t -> property

  (**[goto_height nt] is [Array.length (goto_symbols nt)]. *)
  val goto_height: Nonterminal.t -> int

  (**The string [variant] is "short" or "long" and is printed when
     --timings is enabled. *)
  val variant: string

end

(**This module computes the known suffix of the stack, a sequence of
   symbols, in each of the automaton's states. The length of this
   sequence can be predicted based on the LR(0) items present in this
   state: it is the maximum position of the bullet over all items. *)
module Short () : STACK_SYMBOLS

(* The functor [Short] is used by [Dump]; this call can take place either
   after or before conflicts have been resolved. Regardless of this, one
   call to [Short()] is performed in [StackSymbolsShort], after conflicts
   have been resolved. *)

(**This module computes the known suffix of the stack, a sequence of
   symbols, in each of the automaton's states. The length of this
   sequence is determined by an analysis of the paths in the LR(0)
   automaton. At each state, the sequence computed by [Short] is
   a suffix of the sequence computed by [Long]. *)
module Long () : STACK_SYMBOLS

(* The "long invariant" was used in Menhir until 2012/08/25. However, the
   extra information that it contains, compared to the "short invariant",
   was useless; computing it was a waste of time. As of 2012/08/25, the
   short invariant has been used. As of 2021/05/14, the long invariant
   is re-introduced, for possible use in the new code back-end. *)

(**This utility function prints a sequence of symbols. Every symbol is
   preceded with a space. *)
val print_symbols: Symbol.t array -> string
