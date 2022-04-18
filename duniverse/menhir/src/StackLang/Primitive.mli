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

val registers : primitive -> registers
(**[registers p] is the set of registers referred to (needed) by the
   primitive operation [p]. *)

val apply : bindings -> primitive -> primitive
(**[apply bs prim] constructs the primitive operation [let bs in prim]. *)

val pure : primitive -> bool
(**[pure prim] indicates whether the primitive operation [prim] is pure,
   i.e., has no observable side effects. *)
