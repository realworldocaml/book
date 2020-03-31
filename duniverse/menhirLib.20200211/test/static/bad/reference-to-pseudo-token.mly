%token A B
%start<unit> main
%left foobar
%%
main: A b %prec foobar {}
b: B foobar {}
