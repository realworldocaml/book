%token A B
%start<unit> main
%on_error_reduce bar

%%

main:
  A B {}

bar:
  A {}
