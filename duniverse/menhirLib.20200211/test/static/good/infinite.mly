%token A
%start <unit> dummy

%%

dummy:
  A { () }
| A infinite { () }

infinite:
  A infinite
    { () }

