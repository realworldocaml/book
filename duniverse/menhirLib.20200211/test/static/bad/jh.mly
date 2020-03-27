(* This grammar was accepted by Menhir prior to 2017/12/06,
   even though its expansion does not terminate.
   https://gitlab.inria.fr/fpottier/menhir/issues/4 *)

%token A
%start<unit> main

%%

app(X, Y):
  X(Y) {}

F(U):
  app(F, id(U)) {}

id(V):
  V {}

main:
  F(A) {}
