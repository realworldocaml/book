%token A B C
%attribute list(B) [@foo "bar"] (* the symbol list(B) is never created during expansion, so this declaration is not OK *)
%attribute list(C) [@foo "bar"] (* the symbol list(C) is created during expansion, so this declaration is OK *)
%start<unit> main
%%
main:
  A B list(C) {}
