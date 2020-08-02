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

(* This code analyzes the output of [ocamlc -i] and returns a list
   of identifiers together with their types. Types are represented
   by offsets in the source string. *)

{

  let fail () =
    Error.error [] "failed to make sense of ocamlc's output."

}

let whitespace = [ ' ' '\t' '\n' '\r' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

(* Read a list of bindings. We start immediately after a [val]
   keyword, so we expect either an end marker, or an identifier,
   followed by a colon, followed by a type, followed by another list
   of bindings. In the latter case, we recognize the identifier and
   the colon, record where the type begins, and pass control to
   [type_then_bindings]. *)

rule bindings env = parse
| "menhir_end_marker : int"
    { env }
| whitespace* ((lowercase identchar*) as id) whitespace* ':' whitespace*
    { type_then_bindings env id (Lexing.lexeme_end lexbuf) lexbuf }
| _
| eof
    { fail() }

(* Read a type followed by a list of bindings. *)

and type_then_bindings env id openingofs = parse
| whitespace+ "val" whitespace
    { let closingofs = Lexing.lexeme_start lexbuf in
      bindings ((id, openingofs, closingofs) :: env) lexbuf }
| _
    { type_then_bindings env id openingofs lexbuf }
| eof
    { fail() }

(* Skip up to the first [val] keyword that follows the begin marker,
   and start from there. *)

and main = parse
| _* "val menhir_begin_marker : int" whitespace+ "val" whitespace+
    { bindings [] lexbuf }
| _
| eof
    { fail() }

