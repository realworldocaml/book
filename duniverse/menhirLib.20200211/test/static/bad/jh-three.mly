(* A more complicated variant of jh. *)

%token B
%start<unit> main

%%

app2(F, X, Y):
  F(X, Y) {}

F(A, U):
  app2(F, A, id(U)) {}

id(V):
  V {}

main:
  F(list, B) {}
