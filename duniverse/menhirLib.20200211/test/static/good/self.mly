%token A
%type<unit> s
%start<unit> dummy

%%

dummy: A {}

s: b s {}

b: {}

