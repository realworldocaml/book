%start<int> a
%token A
%%

a: A { $2 }
