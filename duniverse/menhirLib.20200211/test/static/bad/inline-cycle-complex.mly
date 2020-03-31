%start<int> a
%token INT
%%

a: b {}

%inline b: INT c INT {}
%inline c: d INT d {}
%inline d: b {}
