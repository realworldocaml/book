%token <int> INT
%token PLUS TIMES
%start <int> expr

%left PLUS
%left TIMES

%%

expr:
  i = INT
    { i }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }

