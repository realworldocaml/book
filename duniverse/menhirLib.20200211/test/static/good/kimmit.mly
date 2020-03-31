%{
   let x = X 0
%}
%token<int> X
%start<unit> s
%%
s: X {}
