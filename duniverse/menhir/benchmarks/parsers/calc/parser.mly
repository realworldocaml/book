%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start main
%type<int> main

%%

main:
| expr EOL
    { $1 }

expr:
| INT
    { $1 }
| LPAREN expr RPAREN
    { $2 }
| expr PLUS expr
    { $1 + $3 }
| expr MINUS expr
    { $1 - $3 }
| expr TIMES expr
    { $1 * $3 }
| expr DIV expr
    { if $3 = 0 then Int.max_int else $1 / $3 }
| MINUS expr %prec UMINUS
    { - $2 }

