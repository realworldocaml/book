%token A B
(* Forbidden because a start symbol cannot be an application of a
   parameterized symbol: *)
%start<unit> main(A)
%%
main(X):
  X B {}
