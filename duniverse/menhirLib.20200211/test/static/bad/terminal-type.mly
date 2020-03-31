%token A
%type<int> A
%start<unit> t
%%

t: A {}
