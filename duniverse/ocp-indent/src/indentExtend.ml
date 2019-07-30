(**************************************************************************)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

exception Syntax_not_found of string

type t = {
  keywords : (string * Approx_tokens.token) list;
  lexer : (Lexing.lexbuf -> Approx_tokens.token) option
}

let extensions = Hashtbl.create 17

let register name ?(keywords=[]) ?lexer () =
  Hashtbl.add extensions name {keywords;lexer}

let available () =
  Hashtbl.fold (fun name _ acc -> name::acc) extensions []

let find (name : string) =
  try
    Hashtbl.find extensions name
  with Not_found ->
    raise (Syntax_not_found name)

(* predefined extensions *)
open Approx_tokens
let _ =
  register "lwt" ~keywords:[
    "for_lwt", FOR;
    "lwt", LET;
    "match_lwt", MATCH;
    "try_lwt", TRY;
    "while_lwt", WHILE;
    "finally", WITH;  (* -- no equivalence for this one, this is a hack ! *)
  ] ();
  register "mll" ~keywords:[
    "rule", LET;
    "parse", FUNCTION;
  ] ();
  register "stream" ~keywords:[
    "parser", FUNCTION;
  ] ();
  register "cstruct" ~keywords:[
    "cstruct", TYPE;
  ] ();
  register "bitstring" ~keywords:[
    "bitmatch", MATCH;
  ] ();
