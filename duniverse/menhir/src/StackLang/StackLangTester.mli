(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

val test : StackLang.program -> unit
(** [test program] tests the StackLang program [program]. A number of sentences
   are generated, based on the grammar, by exhaustive enumeration and/or random
   sampling. These sentences may be accepted or rejected by the automaton. We
   use the reference interpreter as an oracle, and verify that interpreting the
   StackLang program yields the same outcome. If this test fails, a message is
   displayed and the process is exited. *)
