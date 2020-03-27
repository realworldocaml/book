(* Forbidden because every symbol that appears in a %start or %type
   declaration must have sort "*": *)
%start<unit> main
%%
main(X):
  X {}
