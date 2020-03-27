%token FOO BAR

%%

a(X):
  x = X { x }

foo:
  a(FOO) a(BAR,BAR) { () }
