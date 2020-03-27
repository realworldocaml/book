%start<unit> s
%token A
%%
s: A _1=A { () }
