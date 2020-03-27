%{

open Ast

%}

%token SEP
%token DOT

%token <string> LIDENT
%token EOF

%start <unit> debut

%%



inst :
| i = LIDENT e = expr
    { () }
| SEP i1 = inst i2 = inst
    { () }
| e = expr DOT
    { () }



expr:
| i = LIDENT
    { () }
| e = expr DOT
    { () }
| i = inst DOT e = expr
    { () }

debut:
| expr EOF
    { () }
