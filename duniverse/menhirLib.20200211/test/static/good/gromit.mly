/*********************************************************************************************************************/
/*                                                                                                                   */
/*                                                   Wallace                                                         */
/*                                                                                                                   */
/*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 */
/*                                                                                                                   */
/*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   */
/*                                                                                                                   */
/*********************************************************************************************************************/
/* $Header: /home/pauillac/formel1/fpottier/cvs/gromit/parser.mly,v 1.8 2000/02/11 16:15:36 fpottier Exp $ */

/* Gromit's parser definition. */

%{

open Syntax

let failure index message =
  let location = Parsing.rhs_start index in
  let message = Printf.sprintf "near character %d: %s" location message in
  raise (SyntaxError message)

%}

/* Tokens. */

%token BEGIN
%token COMMA
%token CONSTRUCTOR
%token CONTRAVARIANT
%token COVARIANT
%token END
%token EOF
%token LBRACE
%token LBRACK
%token LATTICE
%token LESS
%token LET
%token <string> LIDENT
%token LPAREN
%token MORE
%token PRINTED
%token PRIORITY
%token QUOTE
%token RBRACE
%token RBRACK
%token RPAREN
%token SIGNATURE
%token <string> UIDENT

/* Entry points. */

%start signature
%type <Syntax.signature> signature

%%

/* Rules. */

signature:
      SIGNATURE BEGIN lattice_list END PRIORITY constructor_list
      { $3, $6 }
  |   SIGNATURE BEGIN lattice_list END error
      { failure 5 "PRIORITY expected" }
  |   SIGNATURE error
      { failure 2 "BEGIN expected" }
  |   error
      { failure 1 "SIGNATURE expected" }
;

lattice_list:
      lattice
      { [ $1 ] }
  |   lattice lattice_list
      { $1 :: $2 }
  |   error
      { failure 1 "END or LATTICE expected" }
;

lattice:
      LATTICE UIDENT BEGIN lattice_component_list END
      { Lattice($2, $4) }
  |   LATTICE UIDENT error
      { failure 3 "BEGIN expected" }
  |   LATTICE error
      { failure 2 "lattice name (uppercase) expected" }
;

lattice_component_list:
      component
      { [ $1 ] }
  |   component lattice_component_list
      { $1 :: $2 }
  |   error
      { failure 1 "CONSTRUCTOR, COVARIANT, CONTRAVARIANT, LET or END expected" }
;

component:
      CONSTRUCTOR UIDENT arity PRINTED label_or_token_list LBRACK QUOTE UIDENT RBRACK
      { ComponentConstructor($2, $3, ($5, $8)) }
  |   CONSTRUCTOR UIDENT arity PRINTED label_or_token_list LBRACK QUOTE UIDENT error
      { failure 9 "']' expected" }
  |   CONSTRUCTOR UIDENT arity PRINTED label_or_token_list LBRACK QUOTE error
      { failure 8 "token name (uppercase) expected" }
  |   CONSTRUCTOR UIDENT arity PRINTED label_or_token_list LBRACK error
      { failure 7 "token (quoted uppercase name) expected" }
  |   CONSTRUCTOR UIDENT arity PRINTED label_or_token_list error
      { failure 6 "'[' expected" }
  |   CONSTRUCTOR UIDENT arity error
      { failure 4 "PRINTED expected" }
  |   CONSTRUCTOR error
      { failure 2 "constructor name (uppercase) expected" }
  |   variance UIDENT LIDENT
      { ComponentLabel($1, SortRegular, $2, $3) }
  |   variance UIDENT error
      { failure 3 "label name (lowercase) expected" }
  |   variance LESS UIDENT MORE LIDENT
      { ComponentLabel($1, SortRow, $3, $5) }
  |   variance LESS UIDENT MORE error
      { failure 5 "label name (lowercase) expected" }
  |   variance LESS UIDENT error
      { failure 4 "'>' expected" }
  |   variance LESS error
      { failure 3 "lattice name (uppercase) expected" }
  |   variance error
      { failure 2 "'<' or lattice name (uppercase) expected" }
  |   LET UIDENT LESS UIDENT
      { ComponentOrdering($2, $4) }
  |   LET UIDENT LESS error
      { failure 4 "constructor name (uppercase) expected" }
  |   LET UIDENT error
      { failure 3 "'<' expected" }
  |   LET error
      { failure 2 "constructor name (uppercase) expected" }
;

arity:
      /* epsilon */
      { [] }
  |   LBRACE label_comma_list RBRACE
      { $2 }
  |   error
      { failure 1 "PRINTED expected" }
;

label_comma_list:
      LIDENT
      { [ $1 ] }
  |   LIDENT COMMA label_comma_list
      { $1 :: $3 }
  |   LIDENT error
      { failure 2 "',' or '}' expected" }
  |   error
      { failure 1 "label name (lowercase) expected" }
;

variance:
      COVARIANT
      { Covariant }
  |   CONTRAVARIANT
      { Contravariant }
;

label_or_token_list:
      first_label_or_token label_or_token_list0
      { $1 :: $2 }
;

label_or_token_list0:
      /* epsilon */
      { [] }
  |   next_label_or_token label_or_token_list0
      { $1 :: $2 }
;

first_label_or_token:
      LIDENT
      { Label ($1, false) }
  |   QUOTE UIDENT
      { Token $2 }
  |   QUOTE error
      { failure 2 "token name (uppercase) expected" }
  |   LPAREN LIDENT RPAREN
      { Label ($2, true) }
  |   LPAREN LIDENT error
      { failure 3 "')' expected" }
  |   LPAREN error
      { failure 2 "label name (lowercase) expected" }
  |   error
      { failure 1 "label name (lowercase), '(' or token (quoted uppercase name) expected" }
;

next_label_or_token: /* Differs only in error handling. */
      LIDENT
      { Label ($1, false) }
  |   QUOTE UIDENT
      { Token $2 }
  |   QUOTE error
      { failure 2 "token name (uppercase) expected" }
  |   LPAREN LIDENT RPAREN
      { Label ($2, true) }
  |   LPAREN LIDENT error
      { failure 3 "')' expected" }
  |   LPAREN error
      { failure 2 "label name (lowercase) expected" }
  |   error
      { failure 1 "label name (lowercase), '(', token (quoted uppercase name) or '[' expected" }
;

constructor_list:
      /* epsilon */
      { [] }
  |   UIDENT constructor_list
      { $1 :: $2 }
  |   error
      { failure 1 "constructor name (uppercase) expected" }

%%
