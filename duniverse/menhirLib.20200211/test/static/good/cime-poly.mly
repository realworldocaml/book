/***************************************************************************

parser for polynomials

CiME Project - Démons research team - LRI - Université Paris XI

$Id: poly_parser.mly,v 1.21 2002/04/12 15:33:58 marche Exp $

***************************************************************************/

%{

  open Abstract_constraint;;

%}


%token <string> IDENT
%token PARGAUCHE PARDROITE SEMICOLON COMMA EOF
%token TRUE FALSE AND OR NOT IMPLIES EQUIV EXISTS FORALL
%token PLUS MINUS EXP MULT DIV VERTICALBAR
%token <string> COMP
%token <Numbers.t> INT


%start constraint_entry
%type  <Abstract_constraint.formula> constraint_entry

%start expr
%type  <Abstract_constraint.expr> expr

%nonassoc EXISTS FORALL
%right IMPLIES EQUIV
%left OR
%left AND
%nonassoc NOT
%left PLUS MINUS
%left MULT DIV
%nonassoc UMINUS
%right EXP

%%

constraint_entry:
| formula EOF { $1 }
;

formula:
| formula AND formula
    { conj $1 $3 }
| formula OR formula
    { disj $1 $3 }
| NOT formula
    { Neg($2) }
| formula IMPLIES formula
    { Implies($1,$3) }
| formula EQUIV formula
    { Equiv($1,$3) }
| EXISTS id_list COMMA formula
    { Exists($2,$4) }
| FORALL id_list COMMA formula
    { Forall($2,$4) }
| expr COMP expr
    { Comp($1,$2,$3) }
| expr VERTICALBAR expr
    { Comp($1,"|",$3) }
| expr COMP expr COMP expr
    { conj (Comp($1,$2,$3)) (Comp($3,$4,$5)) }
| expr COMP expr COMP expr COMP expr
    { conj (conj (Comp($1,$2,$3)) (Comp($3,$4,$5))) (Comp($5,$6,$7)) }
| PARGAUCHE formula PARDROITE
    { $2 }
| TRUE
    { True }
| FALSE
    { False }
;

id_list:
| IDENT
    { [$1] }
| IDENT id_list
    { $1::$2 }
;

expr:
| IDENT
    { Var($1) }
| INT
    { Cte($1) }
| PARGAUCHE expr PARDROITE
    { $2 }
| expr PLUS expr
    { Plus($1,$3) }
| expr MINUS expr
    { Sub($1,$3) }
| MINUS expr %prec UMINUS
    { Minus($2) }
| expr MULT expr
    { Mult($1,$3) }
| expr DIV expr
    { Quotient($1,$3) }
| expr EXP INT
    { try
	power $1 (Numbers.to_int $3)
      with
	Invalid_argument("Numbers.to_int") ->
	  failwith "Exponent too large"
    }
;
