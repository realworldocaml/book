%start<int> a
%token A
%%

a: x = A { $1 }
