%token INT PLUS MINUS
%start <unit>expr
%%

expr:
| PLUS PLUS expr INT {}
| INT                {}
| plus               {}
| indir MINUS indir  {}

indir: plus { }

plus: expr PLUS expr  { }

