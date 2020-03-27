%token A B
%start<unit> main
%type<int> FOO (* does not exist *)

%%

main: A B {}
