%token A B
%start<unit> main
%%
main: A B { $symbolstartpos($2) }
