%token <int> INT
%token PLUS TIMES END
%start <int> main

%left PLUS
%left TIMES

%%

main:
  e = expr END
    { e }

expr:
  i = INT
    { i }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }

