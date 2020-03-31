(* A grammar of arithmetic expressions without semantic actions. *)

%token DIV EOL INT LPAREN MINUS PLUS RPAREN TIMES
%start<unit> main

%%

let main :=
  additive_expr; EOL

let additive_expr :=
| multiplicative_expr
| additive_expr; PLUS; multiplicative_expr
| additive_expr; MINUS; multiplicative_expr

let multiplicative_expr :=
| atomic_expr
| multiplicative_expr; TIMES; atomic_expr
| multiplicative_expr; DIV; atomic_expr

let atomic_expr :=
| LPAREN; additive_expr; RPAREN
| INT
| MINUS; atomic_expr
