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

(** [registers ps] is the set of registers defined by the patterns [ps]. *)
val registers : patterns -> registers

(**[occurs r ps] determines whether the register [r] appears
   in any of the patterns [ps]. *)
val occurs : register -> patterns -> bool

(** [restrict1 rs p] is a copy of the pattern [p] where every register
    that is not in the set [rs] is replaced by a wildcard.  *)
val restrict1 : registers -> pattern -> pattern

(** [restrict rs ps] is a copy of the patterns [ps] where every register
    that is not in the set [rs] is replaced by a wildcard.  *)
val restrict : registers -> patterns -> patterns
