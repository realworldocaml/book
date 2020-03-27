%token A
%start<unit> main

%%

foo(X,X): (* the two formal parameters have the same name *)
  X A {}

main:
  foo(A,A) {}
