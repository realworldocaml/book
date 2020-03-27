%token A B
%start<unit> main
%type<unit> phrase (* ill-kinded: phrase is a parameterized nonterminal symbol *)

%%

main:
  A phrase(B) A {}

phrase(X):
  X* {}
