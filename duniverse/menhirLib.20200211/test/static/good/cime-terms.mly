/***************************************************************************

parser for terms

CiME Project - Démons research team - LRI - Université Paris XI

$Id: term_parser.mly,v 1.21 2003/10/13 11:43:34 marche Exp $

***************************************************************************/

%{

  open Gen_terms
  open Term_syntax
  open User_signatures
  open Finite_orderings

%}


%token <string> PREFIX_IDENT POSTFIX_IDENT INFIX_IDENT
%token <Variables.var_id> VAR_IDENT
%token COMMA SEMICOLON OPENPAR CLOSEPAR ARROW
%token EQ GT LT GE LE NE AND OR NOT
%token MUL LR_LEX RL_LEX
%token EOF

%left OR
%left AND
%nonassoc NOT
%nonassoc EQ GT LT GE LE NE
%left INFIX_IDENT
%nonassoc POSTFIX_IDENT
%nonassoc PREFIX_IDENT

%start term_eof
%type <string Gen_terms.term> term_eof

%start rule_set_eof
%type <(string Gen_terms.term * string Gen_terms.term) list> rule_set_eof

%start equation_set_eof
%type <(string Gen_terms.term * string Gen_terms.term) list> equation_set_eof

%start equation_eof
%type <(string Gen_terms.term * string Gen_terms.term)> equation_eof

%start precedence_eof
%type <string Finite_orderings.precedence list> precedence_eof

%start order_constraint_eof
%type <string Order_constraints.formula> order_constraint_eof

%start status_eof
%type <(string * Rpo.rpo_status) list > status_eof

%%

term_eof:
  term EOF { $1 }
;

term :
| VAR_IDENT
    { make_var_term $1 }
| PREFIX_IDENT
    { !check_make_term $1 [] }
| PREFIX_IDENT term
    { !check_make_term $1 [$2] }
| PREFIX_IDENT OPENPAR term COMMA term_list CLOSEPAR  %prec POSTFIX_IDENT
    { !check_make_term $1 ($3::$5) }
| term POSTFIX_IDENT
    { !check_make_term $2 [$1] }
| OPENPAR term COMMA term_list CLOSEPAR POSTFIX_IDENT
    { !check_make_term $6 ($2::$4) }
| term INFIX_IDENT term
    { !check_make_term $2 [$1;$3] }
| OPENPAR term CLOSEPAR
    { $2 }
;

term_list:
  term                  { [$1] }
| term COMMA term_list  { $1 :: $3 }
;

equation_set_eof:
  equation_set EOF   { $1 }
;

equation_set:
/* epsilon */           { []   }
| equation                  { [$1] }
| equation SEMICOLON equation_set { $1::$3 }
;

equation_eof:
  equation EOF   { $1 }
;

equation:
  term EQ term         { ($1,$3) }
;

rule_set_eof:
  rule_set EOF   { $1 }
;

rule_set:
  /* epsilon */           { []   }
| rule                    { [$1] }
| rule SEMICOLON rule_set { $1::$3 }
;

rule:
  term ARROW term         { ($1,$3) }
;


order_constraint_eof: formula EOF { $1 }
;

formula:
  term comp term           { Order_constraints.Atom($1,$2,$3) }
| formula AND formula      { Order_constraints.conj $1 $3 }
| formula OR formula       { Order_constraints.disj $1 $3 }
| NOT formula              { Order_constraints.Not($2) }
| OPENPAR formula CLOSEPAR { $2 }
;

comp:
  EQ { Order_constraints.Eq }
| GT { Order_constraints.Gt }
| LT { Order_constraints.Lt }
| GE { Order_constraints.Ge }
| LE { Order_constraints.Le }
| NE { Order_constraints.Ne }
;

precedence_eof: precedence EOF
  { $1 }
;

precedence:
  /* epsilon */                 { [] }
| ordered_list  		{ [$1] }
| ordered_list COMMA precedence	{ $1::$3 }
;

ordered_list:
  ident  			{ One($1) }
| ident EQ ordered_list 	{ Eq($1,$3) }
| ident GT ordered_list 	{ Gt($1,$3) }
| ident LT ordered_list 	{ Lt($1,$3) }
;

ident:
  PREFIX_IDENT  { $1 }
| POSTFIX_IDENT { $1 }
| INFIX_IDENT   { $1 }
;

status_eof: status EOF { $1 }
;

status:
  /* epsilon */                 { [] }
| symbol_status                 { [$1] }
| symbol_status SEMICOLON status    { $1::$3 }
;

symbol_status:
  ident MUL    { ($1,Rpo.Multiset) }
| ident LR_LEX { ($1,Rpo.Lr_lexico) }
| ident RL_LEX { ($1,Rpo.Rl_lexico) }
;
