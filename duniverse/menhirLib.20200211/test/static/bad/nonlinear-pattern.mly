%token<int> A B C D
%token EOF
%start<int> main

%%

let twice(x) ==
  x = x; x = x; { x, x }
