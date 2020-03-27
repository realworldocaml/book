/* Test of the new anonymous rule syntax, including
   anonymous rules nested in anonymous rules. */

%token<int> A B C D EOF
%start<int> phrase

%%

%inline id(X):
  x = X { x }

foo:
  A B { 1 }

bar:
  C D { 2 }

phrase:
  y = id(id(x = foo { x } | z = bar { z }))
  t = id(x = foo { x } | id(z = bar { z }) { 2 })
  EOF
    { y + t }
