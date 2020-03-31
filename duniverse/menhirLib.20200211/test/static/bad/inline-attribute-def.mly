%token A B
%start<unit> main
%%
main:
  foo(B) {}

%inline foo [@cost 0] (bar):
  A bar {}
