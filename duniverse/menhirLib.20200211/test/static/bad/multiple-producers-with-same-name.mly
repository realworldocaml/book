%start<int> a
%token A
%%

a: x = A; x = A {}
