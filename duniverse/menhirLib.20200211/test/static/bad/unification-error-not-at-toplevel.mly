%start<int> a
%token A
%%

a: b(c(A)) b(c(A, A)) {}

c(X) : X {}
b(X) : X {}
