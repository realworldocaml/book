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

(**[specialize] specializes certain routines for the situation where the
   [state] register contains a statically known tag. It also performs a
   certain amount of inlining, insofar as it has a beneficial effect on
   specialization itself. [specialize] should be applied to a program
   where very block is reachable. *)
val specialize : program -> program

(**[commute_pushes] moves PUSH instructions forward in the code, in the
   hope that they meet POP instructions and cancel out. It also performs
   a certain amount of inlining, insofar as it has a beneficial effect
   on the movement of PUSH instructions. *)
val commute_pushes : program -> program

(**[remove_unreachable_blocks program] transforms the program [program] by
   removing every unreachable block. *)
val remove_unreachable_blocks : program -> program

(**[inline cautious program] transforms the program [program] by removing
   every unreachable block and by inlining away every label whose in-degree
   is 1, provided inlining is permitted. The rules that restrict inlining
   are roughly as follows: *)

(**- Inlining a routine inside a [casetag] instruction is not permitted.

   - In normal mode (that is, when [cautious] is false), a routine that
     carries the hint [OnlyIfKnownState] cannot be inlined.

   - In cautious mode, a routine can be inlined only if both the source
     and destination blocks carry the hint [NoHint]. In short, this allows
     inlining a [run] routine into a [run] routine, and nothing else. *)

val inline : (* cautious: *) bool -> program -> program
