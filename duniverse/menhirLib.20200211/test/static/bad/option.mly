%token FOO
%start <unit> option

%%

option:
  FOO { () }

