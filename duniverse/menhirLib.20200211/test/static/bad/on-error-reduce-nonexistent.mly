%token A B
%start<unit> main
%on_error_reduce foo

%%

main:
  A B {}
