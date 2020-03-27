/* Test of the new anonymous rule syntax. */

%token<int> A B C D EOF
%start<int list> phrase

%%

phrase:
  xs = list(x = A | x = B | x = C { x })
  ys = list(x = D y = D { x + y })
  EOF
    { xs @ ys }
