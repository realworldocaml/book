/* Test of the new anonymous rule syntax, inside
   a parameterized definition, whose parameters
   are NOT used by the anonymous rule. */

%{ type ('a, 'b) either = Left of 'a | Right of 'b %}
%token<int> A B C D EOF
%start<(int, int) either> phrase
%start<unit> other

%%

%inline mixed_list(X, Y, Z):
  Z
  list(
    x = X { Left  x }
  | y = Y { Right y }
  ) { $1 }

phrase:
  xs = mixed_list(A, B, C)
  ys = mixed_list(A, B, D)
    (* We should obtain only ONE anonymous symbol because
       Z is unused in the anonymous rule above. *)
  EOF
    { xs @ ys }

other:
  seplist(A) EOF {}

(* A list of X's, separated with C's or D's. We should
   obtain a definition of a symbol that expands to C or
   D and is NOT parameterized over X. *)
seplist(X):
  X {}
| X midrule(C {} | D {}) seplist(X) {}
