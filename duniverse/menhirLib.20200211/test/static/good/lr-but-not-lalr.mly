%token A B C D F
%type <unit> s
%start s

%%

s:
  A e C {}
| A f D {}
| B e D {}
| B f C {}

e:
  F {}

f:
  F {}

