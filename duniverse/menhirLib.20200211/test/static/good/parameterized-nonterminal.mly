%token A
%start<int> a
%%

a: b(A) {}

b(X): c(X) d {}

d: A {}
c(X): X {}
