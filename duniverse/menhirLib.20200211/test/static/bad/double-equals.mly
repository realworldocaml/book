%token FOO BAR

%%

prod:
  x == FOO /* intentional syntax error */
    { () }

