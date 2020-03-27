%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <int> main

(* The calc demo, with an anonymous rule for binary operators. *)

%%

main:
| e = expr EOL
    { e }

expr:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr
  op = anonymous(PLUS { (+) } | MINUS { (-) } | TIMES { ( * ) } | DIV { (/) })
  e2 = expr
    { op e1 e2 }
| MINUS e = expr %prec UMINUS
    { - e }

