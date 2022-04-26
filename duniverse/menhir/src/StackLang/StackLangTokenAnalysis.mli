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

(**[transform program] performs a simple forward data flow analysis ao as to
   determine (an overapproximation of) the set of possible tokens stored in
   the [token] register at each program point where this register is defined.
   This information is then used to transform the program: dead branches in
   [ICaseToken] constructs are removed, and [ICaseToken] constructs with only
   one live branch are eliminated. This transformation can create unreachable
   blocks. *)
val transform : program -> program
