%token A B C
%start main
%%
main: x = A | x = B y = C { x }
