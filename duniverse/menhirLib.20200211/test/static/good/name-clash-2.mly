%start <unit> b
%token BAR

%%

b:
  a { $1 }

a:
  BAR { () }

