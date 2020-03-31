%token MINUS N LPAR RPAR
%start s
%type <unit> s

%%

s:
  e
    { () }

e:
  e MINUS t
    { () }
| t
    { () }

t:
  N
    { () }
| LPAR e RPAR
    { () }

