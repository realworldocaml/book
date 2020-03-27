%token A B
%start<unit> main

%%

main:
  A B {}

foo(X):
  {}

(* [foo] is unreachable, and the sort of its parameter [X] is undetermined. *)
