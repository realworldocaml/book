(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Lexing
open Grammar
open StackLang
open StackLangMeasure

(* This module offers an interpreter for the intermediate language StackLang. *)

type outcome =
  | Accept
  | Reject of int
  | AbortBySemanticAction
  (**[Accept] means that the input is accepted. [Reject s] means that the
     input is rejected by the parser in state [s]. [AbortBySemanticAction]
     means that the parser has reduced a semantic action that ends with the
     [error] token, in which case we assume that the semantic action aborts
     the parser. *)

val interpret :
  (* measure: *) measure ->
  (* program: *) program ->
  (* entry:   *) label ->
  (* emit:    *) (string -> unit) ->
  (* lexer:   *) (lexbuf -> Terminal.t) ->
  (* lexbuf:  *) lexbuf ->
                 outcome
(**The parameters are a [measure] record, a StackLang program, a start label,
   a hook [emit] that emits a trace message, a lexer, and a lexing buffer. The
   interpreter can raise an internal exception in case of an abnormal problem,
   and can propagate an exception raised by the lexer. The dynamic execution
   counts in the [measure] record are updated, but the [total] field is not
   adjusted. *)
