(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(* An ocamlyacc-style, or Menhir-style, parser requires access to
   the lexer, which must be parameterized with a lexing buffer, and
   to the lexing buffer itself, where it reads position information. *)

(* This traditional API is convenient when used with ocamllex, but
   inelegant when used with other lexer generators. *)

type ('token, 'semantic_value) traditional =
    (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'semantic_value

(* This revised API is independent of any lexer generator. Here, the
   parser only requires access to the lexer, and the lexer takes no
   parameters. The tokens returned by the lexer may contain position
   information. *)

type ('token, 'semantic_value) revised =
    (unit -> 'token) -> 'semantic_value

(* --------------------------------------------------------------------------- *)

(* Converting a traditional parser, produced by ocamlyacc or Menhir,
   into a revised parser. *)

(* A token of the revised lexer is essentially a triple of a token
   of the traditional lexer (or raw token), a start position, and
   and end position. The three [get] functions are accessors. *)

(* We do not require the type ['token] to actually be a triple type.
   This enables complex applications where it is a record type with
   more than three fields. It also enables simple applications where
   positions are of no interest, so ['token] is just ['raw_token]
   and [get_startp] and [get_endp] return dummy positions. *)

let traditional2revised
  (get_raw_token : 'token -> 'raw_token)
  (get_startp    : 'token -> Lexing.position)
  (get_endp      : 'token -> Lexing.position)
  (parser : ('raw_token, 'semantic_value) traditional)
: ('token, 'semantic_value) revised =

  (* Accept a revised lexer. *)

  fun (lexer : unit -> 'token) ->

    (* Create a dummy lexing buffer. *)

    let lexbuf : Lexing.lexbuf =
      Lexing.from_string ""
    in

    (* Wrap the revised lexer as a traditional lexer. A traditional
       lexer returns a raw token and updates the fields of the lexing
       buffer with new positions, which will be read by the parser. *)

    let lexer (lexbuf : Lexing.lexbuf) : 'raw_token =
      let token : 'token = lexer() in
      lexbuf.Lexing.lex_start_p <- get_startp token;
      lexbuf.Lexing.lex_curr_p <- get_endp token;
      get_raw_token token
    in

    (* Invoke the traditional parser. *)

    parser lexer lexbuf

(* --------------------------------------------------------------------------- *)

(* Converting a revised parser back to a traditional parser. *)

let revised2traditional
  (make_token : 'raw_token -> Lexing.position -> Lexing.position -> 'token)
  (parser : ('token, 'semantic_value) revised)
: ('raw_token, 'semantic_value) traditional =

  (* Accept a traditional lexer and a lexing buffer. *)

  fun (lexer : Lexing.lexbuf -> 'raw_token) (lexbuf : Lexing.lexbuf) ->

    (* Wrap the traditional lexer as a revised lexer. *)

    let lexer () : 'token =
      let token : 'raw_token = lexer lexbuf in
      make_token token lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p
    in

    (* Invoke the revised parser. *)

    parser lexer

(* --------------------------------------------------------------------------- *)

(* Simplified versions of the above, where concrete triples are used. *)

module Simplified = struct

  let traditional2revised parser =
    traditional2revised
      (fun (token, _, _)  -> token)
      (fun (_, startp, _) -> startp)
      (fun (_, _, endp)   -> endp)
      parser

  let revised2traditional parser =
    revised2traditional
      (fun token startp endp -> (token, startp, endp))
      parser

end
