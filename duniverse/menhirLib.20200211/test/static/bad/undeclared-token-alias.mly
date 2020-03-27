%token<int> INT
%token PLUS       "+"
%token MINUS      "-"
%token TIMES      "*"
%token DIV        "/"
%token LPAREN     "("
%token RPAREN     ")"
%token EOL

%left "+" "-"       /* lowest precedence */
%left "*" "/"       /* medium precedence */
%nonassoc UMINUS    /* highest precedence */

%start <int> main

%%

main:
| e = expr EOL
    { e }

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
