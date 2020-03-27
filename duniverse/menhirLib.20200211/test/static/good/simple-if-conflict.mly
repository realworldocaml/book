%token TRUE FALSE
%token IF THEN ELSE
%start <bool> expression

%%

expression:
| TRUE
    { true }
| FALSE
    { false }
| IF b = expression THEN e = expression
    { if b then e else false }
| IF b = expression THEN e1 = expression ELSE e2 = expression
    { if b then e1 else e2 }

