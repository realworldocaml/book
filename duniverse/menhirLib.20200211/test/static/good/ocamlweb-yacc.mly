/*
 * ocamlweb - A WEB-like tool for ocaml
 * Copyright (C) 1999-2001 Jean-Christophe FILLIÂTRE and Claude MARCHÉ
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 */

/*i $Id: yacc_parser.mly,v 1.10 2004/10/12 12:29:19 filliatr Exp $ i*/

/*s In actions, we reuse the location type for lex files. */

%{

  open Lex_syntax
  open Yacc_syntax

  let dummy_loc =
    { start_pos = Lexing.dummy_pos;
      end_pos = Lexing.dummy_pos;
      start_line = 0 ;
      start_col = 0 }

%}

/*s Yacc tokens. */

%token Ttoken Tstart Ttype Tleft Tright Tnonassoc Tprec Terror
%token <Yacc_syntax.ident> Tident
%token <Yacc_syntax.location> Taction Ttypedecl
%token Tor Tsemicolon Tcolon Tmark
%token EOF

%start yacc_definitions
%type <Yacc_syntax.yacc_definitions> yacc_definitions

%%

/*s Start symbol for yacc description files */

yacc_definitions:
  | header tokendecls Tmark rules header EOF
      { { header = $1 ;
	  decls = $2;
	  rules = $4;
	  trailer = $5 } }
;

header :
  | Taction
      { $1 }
  | /* $\varepsilon$ */
    { dummy_loc }
;

/*s Token declarations. */

tokendecls :
  | tokendecl tokendecls
    { $1::$2 }
  | /*epsilon*/
    { [] }
;

tokendecl :
  | Ttoken Ttypedecl idlist
      { Typed_tokens($2,$3) }
  | Ttoken idlist
      { Untyped_tokens($2) }
  | Ttype Ttypedecl idlist
      { Non_terminals_type($2,$3) }
  | Tstart idlist
      { Start_symbols($2) }
  | Tleft idlist
      { Tokens_assoc($2) }
  | Tnonassoc idlist
      { Tokens_assoc($2) }
  | Tright idlist
      { Tokens_assoc($2) }
;

idlist:
  | Tident
    { [$1] }
  | Tident idlist
    { $1 :: $2 }
;

/*s Parsing of rules. */

rules:
  | /* $\varepsilon$ */
    { [] }
  | general_rule rules
    { $1 :: $2 }
;

/*

Ocamlyacc manual asks for a semicolon at end of each rules. But ocamlyacc
accepts if they are missing. We issue a warning for non conformity to
ocamlyacc documentation.

*/
general_rule:
  | rule Tsemicolon
      { $1 }
  | rule
      { Yacc_syntax.issue_warning "ocamlyacc documentation recommends adding a semicolon at end of each grammar rules";
      $1 }
;

rule :
  | Tident Tcolon right_part
    { ($1,$3) }
  | Tident Tcolon Tor right_part
    { ($1,$4) }
;

right_part :
  | word Taction
    { [($1,$2)] }
  | word Taction Tor right_part
    { ($1,$2) :: $4 }
;

word :
  | /* $\varepsilon$ */
    { [] }
  | Tident word
    { $1 :: $2 }
  | Tprec Tident word
    { $2 :: $3 }
  | Terror word
    { $2 }
;
