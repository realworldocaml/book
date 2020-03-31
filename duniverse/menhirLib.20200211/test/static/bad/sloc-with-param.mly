%token A B
%start<unit> main
%%
main: a = A b = B { $sloc(b) }
