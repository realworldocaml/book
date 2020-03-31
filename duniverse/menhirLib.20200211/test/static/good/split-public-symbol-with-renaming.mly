%token A B EOF
%start<unit> main
%%
%inline eps: {}
%public liste(X): eps {}
%public liste(B): A B liste(B) {}
main: liste(A) B EOF {}
