(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLangBasics

(**[registers vs] is the set of the registers that appear in
   the values [vs]. *)
val registers : values -> registers

(**The union of [accu] with [registers [v]]. *)
val add_registers : registers -> value -> registers

(**[occurs r vs] determines whether the register [r] appears
   in any of the values [vs]. *)
val occurs : register -> values -> bool
