/* Test of the new anonymous rule syntax, inside
   a parameterized definition. */

%{ type ('a, 'b) either = Left of 'a | Right of 'b %}
%token<int> A B C D EOF
%start<(int, int) either> phrase

%%

mixed_list(X, Y):
  list(
    x = X { Left  x }
  | y = Y { Right y }
  ) { $1 }

phrase:
  xs = mixed_list(A, B)
  ys = mixed_list(C, D)
  EOF
    { xs @ ys }
