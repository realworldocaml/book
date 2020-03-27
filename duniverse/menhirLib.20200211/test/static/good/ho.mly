%token INT EOF SEMI LBRACE RBRACE COMMA
%start<unit> main

%%

main:
  expr(separated_list, SEMI)
  weird(separated_list, SEMI)
  bizarre(separated_list, SEMI)
  EOF
    {}

expr(seq, sep):
  INT
| LBRACE seq(sep, expr(seq, sep)) RBRACE
    {}

weird(seq, sep):
  INT
| LBRACE seq(sep, weird(seq, COMMA)) RBRACE
    {}

bizarre(seq, sep):
  INT
| LBRACE seq(sep, bizarre(nonseparated_list, sep)) RBRACE
    {}

nonseparated_list(sep, X):
  list(X)
    {}
