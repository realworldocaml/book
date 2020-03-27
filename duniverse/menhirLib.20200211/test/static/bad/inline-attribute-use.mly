%token A B
%start<unit> main
%%
main:
  foo [@cost 0] {}

%inline foo:
  A B {}
