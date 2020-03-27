/***************************************************************************

parser for words and string rewriting systems

CiME Project - Démons research team - LRI - Université Paris XI

$Id: word_parser.mly,v 1.8 2003/10/13 11:43:34 marche Exp $

***************************************************************************/

%{

  open Words
  open Word_syntax
  open String_signatures
  open Finite_orderings

  let word_power = Listutils.power

%}


%token <String_signatures.symbol_id> IDENT
%token SEMICOLON ARROW LPAR RPAR COMMA
%token POWER EQ GT LT
%token <int> INT
%token EOF

%start word_eof
%type <String_signatures.symbol_id Words.word> word_eof

%start rule_set_eof
%type <String_signatures.symbol_id String_rewriting.srs> rule_set_eof

%start precedence_eof
%type <String_signatures.symbol_id Orderings_generalities.ordering> precedence_eof

%%

word_eof:
  word EOF { $1 }
;

word :
  /* epsilon */
    { [] }
| factor word
    { $1 @ $2 }
;

factor:
  IDENT
    { [$1] }
| factor POWER expo
    { word_power $1 $3 }
| LPAR word RPAR
    { $2 }
;

expo:
  INT  { $1 }
| IDENT
      {
	try
	  int_of_string(!current_signature#string_of_symbol $1)
	with
	    _ ->
	      raise (Syntax_error "invalid exponent")
      }
;

rule_set_eof: rule_set EOF { $1 } ;

rule_set:
  /* epsilon */
    { [] }
| rule
    { [$1] }
| rule SEMICOLON rule_set
    { $1::$3 }
;

rule:
  word ARROW word         { ($1,$3) }
;


precedence_eof: precedence EOF
  {
    let order = identity_ordering (Pervasives.compare : symbol_id -> symbol_id -> int)
    in
    let order = List.fold_left add_list order $1
    in compare order
  }
;

precedence:
  /* epsilon */                 { [] }
| ordered_list  		{ [$1] }
| ordered_list COMMA precedence	{ $1::$3 }
;
ordered_list:
  IDENT  			{ One($1) }
| IDENT EQ ordered_list 	{ Eq($1,$3) }
| IDENT GT ordered_list 	{ Gt($1,$3) }
| IDENT LT ordered_list 	{ Lt($1,$3) }
;


