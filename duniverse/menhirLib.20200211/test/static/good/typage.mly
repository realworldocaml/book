/* Un analyseur syntaxique pour mini-ML */

%{
open Records
%}

/* Tokens */
%token COMMA
%token FUN
%token ARROW
%token LET
%token EQUAL
%token IN
%token <string> IDENT
%token <string> OP
%token <int> INT
%token LPAREN
%token RPAREN
%token EOF
%token FST
%token SND
%token LBRACE
%token RBRACE
%token SEMI
%token DOT
%token AT

/* Priorités et associativités -- du moins prioritaire au plus prioritaire */
%right prec_fun
%right prec_let
%right COMMA

/* Points d'entrée */
%type <Records.expression> expression
%start expression

%%

expression: expr EOF            { $1 }
;

expr:
    application                 { $1 }
  | expr COMMA expr             { Paire($1, $3) }
  | FUN IDENT ARROW expr %prec prec_fun
                                { Fun($2, $4) }
  | LET IDENT EQUAL expr IN expr %prec prec_let
                                { Let($2, $4, $6) }
;

application:
    application closed_expr     { App($1, $2) }
  | FST closed_expr             { Fst $2 }
  | SND closed_expr             { Snd $2 }
  | application AT LBRACE IDENT EQUAL expr RBRACE { App (Exten $4, Paire ($1, $6)) }
  | closed_expr                 { $1 }
;

closed_expr:
    IDENT               { Var $1 }
  | OP                  { Op $1 }
  | INT                 { Const $1 }
  | LPAREN expr RPAREN  { $2 }
  | LBRACE entries RBRACE { Enreg $2 }
  | closed_expr DOT IDENT { App (Proj $3, $1) }
;

entries:
    entry { [ $1 ] }
  | entries SEMI entry { $3 :: $1 }
;

entry:
    IDENT EQUAL application { $1, $3 }
;

