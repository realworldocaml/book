%token A
%start<unit> main
%type<int> foo

%%

main: A {}
