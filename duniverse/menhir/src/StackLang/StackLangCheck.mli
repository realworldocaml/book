(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLang

(**[wf program] checks that the program [program] contains no references to
   undefined registers or tags. This check is in principle unnecessary, but
   can be a useful debugging aid. [program] is returned (unchanged). *)
val wf : program -> program

(**A StackLang program is well-typed if: (1) we are able to compute the shape
   of the stack at every point, and to check this shape remains consistent
   with the current state; and (2) we are able to compute the final type at
   every point in a consistent way.

   We do not keep track of the types of the registers. If desired, it would be
   possible to do so; one could distinguish the registers that hold a state
   (only the [state] register), the registers that hold a position, and the
   registers that hold a semantic value for some nonterminal symbol [nt].

   A case analysis on the [state] register, in a situation where the value of
   this register is statically known, is considered ill-typed.

   A provably dead branch, in a case analysis on the [state] register, is
   well-typed, and it is marked dead (that is, its body is replaced with a
   DEAD instruction).

   [wt program] checks that the program [program] is well-typed, and returns a
   program where all provably dead branches have been marked dead. *)
val wt : program -> program
