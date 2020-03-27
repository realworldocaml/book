%start<int> a
%%

a: b {}
%inline b: b {}
