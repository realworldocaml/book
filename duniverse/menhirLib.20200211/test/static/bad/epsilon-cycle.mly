%start a
%type <unit> a
%token B

%%

a:
  b | B { () }

b:
  a { () }

