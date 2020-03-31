%start<int> a
%token T
%%

a: x=b; T b { f_a x }
%inline b: x=c; T { f_b x }
%inline c: x=d; T { f_c x }
d: T {}
