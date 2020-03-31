%token A B C
%start<unit> main
%attribute liste(A) [@liste.A true]
%attribute liste    [@happy true]
%%

main:
  liste(A) liste(B) liste(C)
    {}

liste [@liste true] (X):
  x = X xs = liste(X) { x :: xs }
|                     { [] }
