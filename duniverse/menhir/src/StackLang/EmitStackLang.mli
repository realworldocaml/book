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

(**This module creates a StackLang program, based on information about the
   grammar and about the LR(1) automaton that is extracted from the modules
   Grammar, Lr1, and Default. *)
module Run () : sig

  val program : program

end

(* Naming conventions. *)

(**The register that holds the current state of the automaton. Only this
   register can hold a state. It is not necessarily live at all times. *)
val state : register

(**The register that holds the lexer. *)
val lexer : register

(**The register that holds the lexbuf. *)
val lexbuf: register

(**The register that holds the current token. *)
val token : register

(**The registers that must be defined (with dummy values) in the initial
   runtime environment in StackLangInterpreter. This is not very pretty, but
   works. It is used as part of our testing infrastructure for StackLang. *)
val required : register list
