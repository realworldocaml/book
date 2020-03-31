%start<unit> s
%token<unit> A
%%
s : A _1=A  {}
