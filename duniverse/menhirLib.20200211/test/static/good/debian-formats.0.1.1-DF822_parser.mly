(* Original file: debian-formats.0.1.1/ocaml-debian-formats-0.1.1/src/DF822_parser.mly *)
/******************************************************************************/
/*  ocaml-debian-formats: parse Debian files.                                 */
/*                                                                            */
/*  Copyright (C) 2010-2017, Sylvain Le Gall                                  */
/*                                                                            */
/*  This library is free software; you can redistribute it and/or modify it   */
/*  under the terms of the GNU Lesser General Public License as published by  */
/*  the Free Software Foundation; either version 2.1 of the License, or (at   */
/*  your option) any later version, with the OCaml static compilation         */
/*  exception.                                                                */
/*                                                                            */
/*  This library is distributed in the hope that it will be useful, but       */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of                */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         */
/*  COPYING for more details.                                                 */
/*                                                                            */
/*  You should have received a copy of the GNU Lesser General Public License  */
/*  along with this library; if not, write to the Free Software Foundation,   */
/*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             */
/******************************************************************************/

/* RFC822-like parser

  This parser handle the surface syntax of CUDF documents: it recognizes
  RFC822 stanzas, folds together line continuations, and throws away comments
  and empty lines
*/

%{

open ExtLib

exception Dup_stanza

let extend_loc (r1_start, _r1_end) (_r2_start, r2_end) = 
  (r1_start, r2_end)

let loc_of_lexbuf b = 
  (b.Lexing.lex_start_p, b.Lexing.lex_curr_p)

let join (r1, v) (r2, cont) = 
  extend_loc r1 r2, v ^ cont

%}

%token <string * (DFUtils.loc * string)> FIELD
%token <DFUtils.loc * string> CONT
%token EOL EOF
%type <(string * (DFUtils.loc * string)) list list> doc_822
%type <(string * (DFUtils.loc * string)) list option> stanza_822
%start doc_822 stanza_822

%%

doc_822:
  | stanzas 		{ $1 }
  | eols stanzas	{ $2 }
;

stanza_822:
  | stanza	{ Some $1 }
  | eols stanza	{ Some $2 }
  | eols EOF	{ None }
  | EOF		{ None }
;

eols:
  | EOL		{}
  | EOL eols	{}
;

stanzas:
  | 			{ [] }
  | stanza EOF		{ [ $1 ] }
  | stanza eols stanzas	{ $1 :: $3 }
;

stanza:
  | fields	{ let keys = List.map fst $1 in
		  (* check for re-defined keys *)
		  if List.length (List.unique keys) < List.length keys then
		    raise Dup_stanza
		  else
		    $1
		}
;

fields:
  | field		{ [ $1 ] }
  | field fields	{ $1 :: $2 }
;

field:
  | FIELD EOL		{ $1 }
  | FIELD EOL linecont	{ let k, v = $1 in
			  k, (join v $3) }
;

linecont:
  | CONT EOL		{ $1 }
  | CONT EOL linecont	{ join $1 $3 }
;

%%

let error_wrapper f =
  fun lexer lexbuf ->
    try
      f lexer lexbuf
    with 
      | Parsing.Parse_error ->
	  raise (DFUtils.Parse_error_822
		   ("RFC 822 (stanza structure) parse error",
		    loc_of_lexbuf lexbuf))
      | Dup_stanza ->
	  raise (DFUtils.Parse_error_822
		   ("duplicate keys in stanza",
		    loc_of_lexbuf lexbuf))

let doc_822 = error_wrapper doc_822
let stanza_822 = error_wrapper stanza_822
