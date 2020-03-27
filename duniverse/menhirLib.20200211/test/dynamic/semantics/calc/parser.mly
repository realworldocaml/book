%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token DOT

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <int> main

%%

main:
| e = expr EOL
    { e }

/* Added just to exercise productions with an empty right-hand side. */
nothing:
| /* nothing */
    { () }

/* Added just to exercise productions with an empty right-hand side, in a choice. */
optional_dot:
| /* nothing */
    { () }
| DOT
    { () }

expr:
| i = INT
    { i }
| LPAREN nothing e = expr RPAREN
    { e }
| e1 = expr PLUS optional_dot e2 = expr
    { e1 + e2 }
| e1 = expr MINUS optional_dot e2 = expr
    { e1 - e2 }
| e1 = expr TIMES optional_dot e2 = expr
    { e1 * e2 }
| e1 = expr DIV optional_dot e2 = expr
    { e1 / e2 }
| MINUS e = expr %prec UMINUS
    { - e }

