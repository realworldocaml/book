%start<int> a
%token A
%%

a: b(A, A) {}

b(X): X {}
