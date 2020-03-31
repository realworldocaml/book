%token A B C
%start<unit> main
%on_error_reduce phrase(foo)

%%

main:
  A phrase(B) C {}

phrase(X):
  X* {}
