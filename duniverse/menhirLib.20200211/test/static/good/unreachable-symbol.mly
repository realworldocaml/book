%token FOO
%start main
%type <unit> main

%%

main:
  FOO
    { () }

bar:
  main
    { $1 }

