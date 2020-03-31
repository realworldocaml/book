%start<int> main
%token A

%%

main:
  A { 0 };

%type<int> main
