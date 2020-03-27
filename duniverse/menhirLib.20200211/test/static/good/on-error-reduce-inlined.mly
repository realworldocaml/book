%token A B
%start<unit> main
%on_error_reduce ioption(A)

%%

main:
  ioption(A) B {}
