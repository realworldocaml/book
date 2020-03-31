%type<bool> a
%start<int> b
%%
b: a {}
%public a: {}
