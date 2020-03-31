%start<int> a
%%

a: b {}
%inline b: c {}
%inline c: b {}
