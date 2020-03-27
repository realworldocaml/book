%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <int> main

%%

main:
| e = expr EOL
    { e }

expr:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| expr PLUS expr
    { $1 + $3 }
    (* We use dollars and --no-dollars is enabled; this should fail. *)
| e1 = expr MINUS e2 = expr
    { e1 - e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }
| e1 = expr DIV e2 = expr
    { e1 / e2 }
| MINUS e = expr %prec UMINUS
    { - e }
