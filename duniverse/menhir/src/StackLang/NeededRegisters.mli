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

(**[update prog] annotates every typed block in the program [prog] with the
   set of the registers that it needs. This information is assumed to be
   initially missing: that is, the field [needed] in every typed block is
   assumed to contain a dummy value. This information is computed via a global
   analysis, and every [needed] field is then updated. At the same time, all
   useless DEF or PRIM instructions are removed. *)
val update: program -> program

(* One might wish to place the burden of initializing the [needed] fields with
   correct data on the module [EmitStackLang], thus removing the need for this
   analysis. Experience shows that this is heavy, and leads to unmaintainable
   code there. *)
