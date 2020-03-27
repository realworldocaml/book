%start<int> a
%token I
%%
a: b {}
%public %inline b: I {}
