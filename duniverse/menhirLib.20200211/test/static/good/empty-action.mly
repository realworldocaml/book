%token A
%start<int> a
%%

a: b c
{}

%inline b: A {}

c: A {}
