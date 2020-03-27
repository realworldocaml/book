(* A more complicated variant of jh. *)

%token A B
%start<unit> main

%%

app2(F, X, Y):
  F(X, Y) {}

F(X, U):
  app2(F, id(X), U) {}

id(V):
  V {}

main:
  F(A, B) {}
