%start<int> a
%token A
%%

a: b(c, c) b(A, A) {}


b(X, Y): X(Y(A)) {}

c(X): X {}
