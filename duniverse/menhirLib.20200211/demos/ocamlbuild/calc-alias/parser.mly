(* In the following, we exploit token aliases: e.g., "+" is an alias
   for PLUS, "-" is an alias for MINUS, and so on. We exhibit a tiny
   bit of bad taste and define "42" as an alias for the token INT.
   Obviously there are integers other than 42; the token INT and its
   alias "42" stand for an arbitrary integer literal. *)

(* Token, and token aliases, are declared here: *)

%token<int> INT  "42"
%token PLUS       "+"
%token MINUS      "-"
%token TIMES      "*"
%token DIV        "/"
%token LPAREN     "("
%token RPAREN     ")"
%token EOL

(* Token aliases can be used throughout the rest of the grammar. E.g.,
   they can be used in precedence declarations: *)

%left "+" "-"       /* lowest precedence */
%left "*" "/"       /* medium precedence */
%nonassoc UMINUS    /* highest precedence */

%start <int> main

%%

main:
| e = expr EOL
    { e }

(* Token aliases can also be used inside rules: *)

expr:
| i = "42"
    { i }
| "(" e = expr ")"
    { e }
| e1 = expr "+" e2 = expr
    { e1 + e2 }
| e1 = expr "-" e2 = expr
    { e1 - e2 }
| e1 = expr "*" e2 = expr
    { e1 * e2 }
| e1 = expr "/" e2 = expr
    { e1 / e2 }
| "-" e = expr %prec UMINUS
    { - e }
