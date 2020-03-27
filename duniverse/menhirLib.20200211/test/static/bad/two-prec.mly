%token A B
%start<unit> main

%left foo
%left bar

%%

main:
  A B %prec foo {} %prec bar
