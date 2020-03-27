%start<int> a
%token<int> T
%%

a: b c d c { $2 + $3 + $4 }

%inline b: T T {}
c: x=T { x + 1 }
%inline d: { 0 }
