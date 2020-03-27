%token<int> A B C D
%token EOF
%start<int> main

%%

let twice(foo, bar) ==
  (x, y) = foo; (y, z) = bar; { x, y, z }
  (* Note [y] is bound twice. *)
