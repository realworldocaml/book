%token A B
%start<unit> main
%on_error_reduce phrase

%%

main:
  A phrase(B) {}

phrase(X):
  X* {}
