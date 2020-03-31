(* Another (artificial) example where the parameters are exchanged
   in the recursive call. *)

%token A B
%start<unit> main

%%

F(X,Y):
  B         {}
| A F(Y, X) {}

id(X):
  X {}

main:
  F(A, id(A)) {}
