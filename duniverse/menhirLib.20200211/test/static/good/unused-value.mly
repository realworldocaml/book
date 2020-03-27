%token FOO BAR
%start <unit> main

%%

main:
  f = FOO b = BAR
    { () }

