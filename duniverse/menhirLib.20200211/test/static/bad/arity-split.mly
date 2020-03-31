%token A B
%start<unit> main
%%
main: foo {}

%public foo   : A {}
%public foo(X): X {}
