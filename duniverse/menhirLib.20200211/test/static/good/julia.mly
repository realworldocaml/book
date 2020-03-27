%{ %}

%token TIf TElse TOPar TCPar TReturn TIdent
%token EOF

%nonassoc TIf
%nonassoc TElse

%start main
%type <void> main

%%

main:
   statement EOF { }
statement:
  TReturn
  {  }
| TIf TOPar expr TCPar statement %prec TIf
  {  }
| TIf TOPar expr TCPar statement TElse statement
  {  }

expr:
  TIdent { }

