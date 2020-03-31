/* This parser is done to match the interface with Coq that was previously done for the ELAN system */

%{
  open Coq_syntax
  open Gen_terms

  let current_variables = new vars
  let opset = new operators
%}

%token COMMA COLON LEFT_PAR RIGHT_PAR RIGHTARROW EQUAL NEWLINE
%token SEARCH THEORY SORT FUNCTION RULE END TERM QUIT ERROR
%token <string> IDENT

%start search
%type <string> search

%start theory_oe
%type <string * (string list * int) list * (string * string Gen_terms.term * string Gen_terms.term) list> theory_oe

%start term_oe
%type <string Gen_terms.term> term_oe

%%

search :
| SEARCH IDENT comment
    { $2 }
| ERROR comment
    { raise (Client_error $2) }
;

theory_oe :
/* On abandonne le SORT */
| THEORY IDENT comment sortp signature rule_set END comment
    { ($2, $5, $6) }
| ERROR comment
    { raise (Client_error $2) }
;

term_oe :
| TERM comment term NEWLINE END
    { $3 }
| ERROR comment
    { raise (Client_error $2) }
| QUIT comment
    { raise Quit }
;

comment :
| NEWLINE
    { "" }
| IDENT comment
    { $1^" "^$2 }
| punctuation comment
    { $1^" "^$2 }

punctuation :
| RIGHT_PAR
    { ")" }
| LEFT_PAR
    { "(" }
| COLON
    { ":" }
| COMMA
    { "," }

term :
| LEFT_PAR IDENT term_seq RIGHT_PAR /* all operators are prefix */
    { make_term hash_consing_table $2 $3 }
| IDENT
    { if opset#contains $1 then
	make_term hash_consing_table $1 []
      else
	make_var_term hash_consing_table (current_variables#var_of_string $1) }
;

term_seq :
| term
    { [$1] }
| term term_seq /* arguments are sequentially listed, separated by spaces */
    { $1::$2 }
;

sortp :
| sort
    { [$1] }
| sort sortp
    { $1::$2 }
;

sort :
  SORT ident_seq NEWLINE
    { $2 }
;

ident_seq :
| IDENT
    { [$1] }
| IDENT COMMA ident_seq
    { $1::$3 }
;

signature :
| symbol
    { [$1] }
| symbol signature
    { $1::$2 }
;

symbol :
  FUNCTION ident_seq COLON types NEWLINE
    { opset#union $2;
      ($2, $4) }
;

types :
| IDENT
    { 0 }
| IDENT RIGHTARROW types
    { 1+$3 }

rule_set :
| rule
    { [$1] }
| rule rule_set
    { $1::$2 }
;

rule :
  RULE IDENT term EQUAL term NEWLINE
    { ($2, $3, $5) }
;
