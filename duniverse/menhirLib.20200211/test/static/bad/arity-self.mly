%token FOO BAR
%start<unit> main

%%

main:
  seq(FOO) BAR {}

seq(X):
  /* nothing */
    { [] }
| x = X; xs = seq(X, BAR)
    { x :: xs }
