(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This is the lattice of the natural numbers, completed with [infinity], and
   ordered towards infinity: thatis, [bottom] is zero, [top] is infinity. *)

(* Please be aware that this lattice has unbounded height, so a fixed point
   computation in this lattice does not necessarily terminate. *)

type property

val bottom: property
val infinity: property
val finite: int -> property

val equal: property -> property -> bool
val is_maximal: property -> bool

val max: property -> property -> property
val add: property -> property -> property

val max_lazy: property -> (unit -> property) -> property
val add_lazy: property -> (unit -> property) -> property

val print: property -> string
val to_int: property -> int
