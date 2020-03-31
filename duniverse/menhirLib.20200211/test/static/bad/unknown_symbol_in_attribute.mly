%token A B C
%attribute list(D) [@foo "bar"]       (* the symbol D is unknown, so this %attribute declaration cannot work *)
%start<unit> main
%%
main:
  A B list(C) {}
