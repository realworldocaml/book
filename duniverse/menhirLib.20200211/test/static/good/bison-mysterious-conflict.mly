/* This grammar is not LALR(1). It is taken from the Bison manual. */

%token ID COMMA COLON
%start <unit> def

%%

def:
  param_spec return_spec COMMA
    { () }

param_spec:
  typ
    { () }
| separated_nonempty_list(COMMA, name) COLON typ
    { () }

return_spec:
  typ
    { () }
| name COLON typ
    { () }

typ:
  ID
    { () }

name:
  ID
    { () }

