/***************************************************************************

The parser for parametrized signatures, strings and string rewriting systems.

CiME Project - Démons research team - LRI - Université Paris XI

$Id: parameterized_signatures_parser.mly,v 1.14 2002/04/23 08:36:53 marche Exp $

***************************************************************************/

%{

  open Parameterized_signatures
  open Parameterized_signatures_syntax

%}


%token <string> FORMULA
%token SEMICOLON
%token PIPE
%token <string> IDENT
%token EOF
%token POWER FP LPAR RPAR ARROW

%start signature_eof
%type <Parameterized_signatures_syntax.signature> signature_eof

%start cword_eof
%type <Parameterized_signatures_syntax.constrained_word> cword_eof

%start rules_eof
%type <Parameterized_signatures_syntax.rules> rules_eof

%%

/* This part deals with signatures */

signature_eof:
  | EOF {[]}
  | signature EOF {$1}
;

signature:
  | elt {[$1]}
  | elt SEMICOLON {[$1]}
  | elt SEMICOLON signature {$1::$3}
;

elt:
  | IDENT expr_l
      {$1,$2,Abstract_constraint.True}
  | IDENT expr_l PIPE constr
      {$1,$2,$4}
;

/* This part deals with words. */

cword_eof:
  | cword EOF {$1}
;

cword:
  | word
      { ($1,Abstract_constraint.True) }
  | word PIPE constr
      { ($1,$3) }
;

word:
  | /* epsilon */
    { [] }
  | factor word
    { $1::$2 }
  | simple_word word_no_simple
    { Simple($1)::$2 }
;

word_no_simple:
  | /* epsilon */
    { [] }
  | factor word
    { $1::$2 }
;

factor:
  | letter POWER expr
      { Exp([$1],$3) }
  | LPAR simple_word RPAR POWER expr
      { Exp($2,$5) }
  | FP FORMULA expr expr LPAR simple_word RPAR
      { Product($2,$3,$4,$6) }
  | FP FORMULA expr expr letter
      { Product($2,$3,$4,[$5]) }
;

simple_word:
  | letter
      { [$1] }
  | simple_word letter
      { $1@[$2] }
;

letter:
  | IDENT expr_l
      { ($1,$2) }
;

/* This part deals with rules. */

rules_eof:
  | rules EOF
      { $1 }
;

rules:
  | /* epsilon */
      { [] }
  | rule
      { [$1] }
  | rule SEMICOLON rules
      { $1 :: $3 }
;

rule:
  | word ARROW word PIPE constr
      { ($1,$3,$5) }
  | word ARROW word
      { ($1,$3,Abstract_constraint.True) }
;

/* This part deals with formulae and expressions. */

expr_l:
  | /* epsilon */
      { [] }
  | expr expr_l
      { $1::$2 }
;

expr:
  | FORMULA {Poly_syntax.expr_of_string $1}
;

constr:
  | FORMULA {Poly_syntax.constraint_of_string $1}
;

