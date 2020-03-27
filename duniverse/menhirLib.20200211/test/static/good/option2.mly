%token FOO
%start <unit> main

%%

main:
  o = option { o }

option:
  FOO { () }

