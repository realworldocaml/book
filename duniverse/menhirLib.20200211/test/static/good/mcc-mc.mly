/*
 * Parser for mc files.
 * Currently, it produces FC parse trees. This will probably
 * change.
 *
 * ----------------------------------------------------------------
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Adam Granicz
 * granicz@cs.caltech.edu
 */

%{
open Fc_parse_type
open Fc_frontends

let parse_pascal buffer =
    FrontEnd.set_pascal_parsing ();
    Fc_parse_state.init_types ();
    let elist = Pasqual_parser.bootstrap Pasqual_lexer.main (Lexing.from_string buffer) in
	[PascalExpr (Fc_parse_state.current_position (), elist)]

let parse_pasqual buffer =
    FrontEnd.set_pasqual_parsing ();
    Fc_parse_state.init_types ();
    Pasqual_parser.bootstrap Pasqual_lexer.main (Lexing.from_string buffer)

let parse_fc buffer =
    FrontEnd.set_parameter_copying CopyNone;
    Fc_parse_state.init_types ();
    Fc_parse.prog Fc_lex.main (Lexing.from_string buffer)

%}

%token <Fc_parse_type.pos> TokLt
%token <Fc_parse_type.pos> TokGt
%token <Fc_parse_type.pos> TokEq
%token <Fc_parse_type.pos> TokDQuote

%token <Fc_parse_type.pos> TokPascal
%token <Fc_parse_type.pos> TokPasqual
%token <Fc_parse_type.pos> TokFC

%token TokEof
%token <Fc_parse_type.pos> TokLanguage
%token <Fc_parse_type.pos> TokEndLanguage
%token <Fc_parse_type.pos> TokSource

%token <string * Fc_parse_type.pos> TokString
%token <string * Fc_parse_type.pos> TokSourceString
%token <Symbol.symbol * Fc_parse_type.pos> TokId

%start program
%type <Fc_parse_type.expr list> program
%%

program:
      source_block_list TokEof	{ $1 }
    | source_block_list		{ $1 }
    | TokEof			{ [] }

source_block_list:
      source_block_list source_block
    				{ $1 @ $2 }
    | source_block		{ $1 }

source_block:
      pascal_block		{ $1 }
    | pasqual_block		{ $1 }
    | fc_block			{ $1 }
    | generic_block		{ $1 }

pascal_block:
      TokLt TokLanguage TokEq TokPascal TokSource TokEq TokSourceString TokGt
    				{ parse_pascal (fst $7) }

pasqual_block:
      TokLt TokLanguage TokEq TokPasqual TokSource TokEq TokSourceString TokGt
				{ parse_pasqual (fst $7) }

fc_block:
      TokLt TokLanguage TokEq TokFC TokSource TokEq TokSourceString TokGt
				{ parse_fc (fst $7) }

generic_block:
      TokLt TokLanguage TokEq TokString TokSource TokEq TokSourceString TokGt
    				{ match String.lowercase (fst $4) with
				      "pascal" ->
					parse_pascal (fst $7)
				    | "pasqual" ->
					parse_pasqual (fst $7)
				    | "c" | "fc" ->
					parse_fc (fst $7)
				    | _ ->
					print_string ("warning: block ignored (no front-end is found for " ^ (fst $4) ^ ")\n");
					[]
				}
