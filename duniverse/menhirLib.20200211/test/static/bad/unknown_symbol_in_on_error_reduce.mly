%token A B
%start<unit> main
%on_error_reduce FOO (* does not exist *)

%%

main: A B {}
