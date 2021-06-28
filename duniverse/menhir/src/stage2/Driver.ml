(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* The module [Driver] serves to offer a unified API to the parser,
   which could be produced by either ocamlyacc or Menhir. *)

(* This is the Menhir-specific driver. We wish to handle syntax errors
   in a more ambitious manner, so as to help our end users understand
   their mistakes. *)

open Parser.MenhirInterpreter (* incremental API to our parser *)

(* [fail buffer lexbuf s] is invoked if a syntax error is encountered
   in state [s]. *)

let fail buffer lexbuf (s : int) =
  (* Display a nice error message. In principle, the table found in
     [ParserMessages] should be complete, so we should obtain
     a nice message. If [Not_found] is raised, we produce a generic
     message, which is better than nothing. Note that the OCaml code
     in [ParserMessages] is auto-generated based on the table in
     [ParserMessages.messages]. *)
  let message =
    try
      ParserMessages.message s
    with Not_found ->
      Printf.sprintf "Unknown syntax error (in state %d).\n" s
  in
  (* Show the two tokens between which the error took place. *)
  let where = MenhirLib.ErrorReports.show InputFile.chunk buffer in
  (* Hack: remove the final newline, because [Error.error] adds one. *)
  let message = String.sub message 0 (String.length message - 1) in
  (* Display our message and die. *)
  Error.error (Positions.lexbuf lexbuf) "syntax error %s.\n%s" where message

(* Same as above, except we expect a checkpoint instead of a state [s]. *)

let fail buffer lexbuf checkpoint =
  match checkpoint with
  | HandlingError env ->
      let s = current_state_number env in
      fail buffer lexbuf s
  | _ ->
      assert false (* this cannot happen *)

(* The entry point. *)

let grammar lexer lexbuf =

  (* Keep track of the last two tokens in a buffer. *)
  let buffer, lexer = MenhirLib.ErrorReports.wrap lexer in

  loop_handle
    (fun v -> v)
    (fail buffer lexbuf)
    (lexer_lexbuf_to_supplier lexer lexbuf)
    (Parser.Incremental.grammar lexbuf.Lexing.lex_curr_p)
