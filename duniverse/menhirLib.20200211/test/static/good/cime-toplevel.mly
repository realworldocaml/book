/***************************************************************************

Parser for toplevel input

CiME Project - Démons research team - LRI - Université Paris XI

$Id: toplevel_parser.mly,v 1.17 2002/12/05 10:43:43 contejea Exp $

***************************************************************************/

%{

open Abstract_syntax;;

%}



%token <string> IDENT
%token <Numbers.t> INTEGER
%token <string> STRING
%token LET FUN ARROW
%token IF THEN ELSE
%token AND OR NOT TRUE FALSE
%token PLUS STAR MINUS
%token CONCAT
%token GE GT LE LT NEQ
%token LEFTPAR RIGHTPAR SEMICOLON COMMA LEFTBRACE RIGHTBRACE
%token EQUAL
%token DIRECTIVE
%token EOF

%start command
%type  <Abstract_syntax.abstract_command> command

%nonassoc ARROW IF
%left OR
%left AND
%left NOT
%left GE GT LE LT NEQ EQUAL
%left PLUS MINUS
%left STAR
%nonassoc UMINUS
%nonassoc IDENT INTEGER STRING FUN LEFTPAR LEFTBRACE CONCAT TRUE FALSE
%left APPLY


%%

command:
  EOF                   { raise End_of_file }
| command_aux SEMICOLON { $1 }
;

command_aux:
| LET IDENT EQUAL expr          { Def($2,$4) }
| LET LEFTPAR identlist RIGHTPAR EQUAL expr          { Deftuple($3,$6) }
| LET FUN IDENT args EQUAL expr { Deffun($3,$4,$6) }
| expr                          { Eval($1) }
| DIRECTIVE IDENT               { Directive($2,"") }
| DIRECTIVE IDENT STRING        { Directive($2,$3) }
| DIRECTIVE IDENT IDENT         { Directive($2,$3) }
| DIRECTIVE IDENT INTEGER       { Directive($2,Numbers.to_string $3) }
;

identlist:
| IDENT                           { [$1] }
| IDENT COMMA identlist           { $1::$3 }
;

args:
  IDENT                     { [$1] }
| IDENT args                { $1::$2 }
;

expr:
| IF expr THEN expr ELSE expr %prec IF
    { If($2,$4,$6) }
| expr simple_expr %prec APPLY
    { Apply($1,$2) }
| FUN IDENT ARROW expr
    { Fun($2,$4) }
| expr PLUS expr
    { Apply(Apply(Var("+"),$1),$3) }
| expr MINUS expr
    { Apply(Apply(Var("-"),$1),$3) }
| MINUS expr %prec UMINUS
    { Apply(Var("_minus"),$2) }
| expr STAR expr
    { Apply(Apply(Var("*"),$1),$3) }
| expr EQUAL expr
    { Apply(Apply(Var("="),$1),$3) }
| expr NEQ expr           { Apply(Apply(Var("<>"),$1),$3) }
| expr GE expr            { Apply(Apply(Var(">="),$1),$3) }
| expr GT expr            { Apply(Apply(Var(">"),$1),$3) }
| expr LE expr            { Apply(Apply(Var("<="),$1),$3) }
| expr LT expr            { Apply(Apply(Var("<"),$1),$3) }
| expr AND expr           { Apply(Apply(Var("and"),$1),$3) }
| expr OR expr            { Apply(Apply(Var("or"),$1),$3) }
| expr CONCAT expr            { Apply(Apply(Var("^"),$1),$3) }
| NOT expr                { Apply(Var("not"),$2) }
| simple_expr
      { $1 }
;

simple_expr:
| IDENT
    { Var($1) }
| INTEGER
    { Integer($1) }
| TRUE
    { Bool(true) }
| FALSE
    { Bool(false) }
| STRING
    { String($1) }
| LEFTPAR expr RIGHTPAR
    { $2 }
| LEFTPAR expr COMMA commalist RIGHTPAR
      { Tuple($2::$4) }
| LEFTBRACE RIGHTBRACE
	  { Set([]) }
| LEFTBRACE semicolonlist RIGHTBRACE
	  { Set($2) }
;

commalist:
| expr
    { [$1] }
| expr COMMA commalist
    { $1::$3 }
;

semicolonlist:
| expr
    { [$1] }
| expr SEMICOLON
    { [$1] }
| expr SEMICOLON semicolonlist
    { $1::$3 }
;

