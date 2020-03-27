%start<int> a
%token A
%%

a: b(A) b(A, A) {}

b(X): X {}
