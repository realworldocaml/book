%token FOO BAR
%start <unit> main

main:
  FOO BAR
    { () }

