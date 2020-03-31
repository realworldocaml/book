%token LPAREN RPAREN MATRIX TIMES INT PLUS TENSOR
%start<int> exp
%%

exp:
  term {}

fa1:
  LPAREN exp RPAREN {}
| INT {}

fa4:
  MATRIX {}
| LPAREN exp RPAREN {}

product:
  product TIMES fa1 {}
| fa1 {}

tensorproduct:
  tensorproduct TENSOR fa4 {}
| fa4 {}

term:
  term PLUS product {}
| term PLUS tensorproduct {}
| product {}
| term PLUS tensorproduct {}
