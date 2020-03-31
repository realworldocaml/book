%token<int> A B C D
%token EOF
%start<int> main

%%

let twice(x) ==
  ~ = x; x = x; { x, x }
  (* Note that, because of the pun on [x],
     [x] is bound twice here. *)
