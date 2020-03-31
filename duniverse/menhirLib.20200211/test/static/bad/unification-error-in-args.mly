%start<int> a
%token A
%%

a: b(A) {}

b(X): X(A) {}
