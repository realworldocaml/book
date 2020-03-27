%token<int> T
%start<int> a
%%

a: x = c; y = b { x + y }
c: x = T { x }
%inline b : x = c { x }
