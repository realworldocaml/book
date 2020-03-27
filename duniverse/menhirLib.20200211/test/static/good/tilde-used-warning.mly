%token<int> A B C D
%token EOF
%start<int> main

%%

let paire(x, y) ==
  ~ = x; ~ = y; { x, y }

let fst(p) :=
  (~, _) = p; <>

let snd(p) :=
  (_, ~) = p; <>

let bizarre_fst(p) :=
  (x, ~) = p; {x}
  (* This use of ~ should trigger a warning. *)

let hop(p) :=
  (~, ~) = p; {()}
  (* This use of ~ should trigger a warning with two positions. *)

let a :=
  A

let b :=
  B

let odd :=
  ~ = a; b
  (* This use of ~ does not trigger a warning,
     because it is a pun -- [~] is sugar for [a].
     A warning will be emitted by OCaml because
     [a] is unused. *)

let main :=
  hop(paire(A, B));
  (a, b) = paire(A, B);
  c = fst(paire(A, B));
  _ = bizarre_fst(paire(A, B));
  d = snd(paire(A, B));
  odd;
  ~ = C;
  (* This use of ~ should trigger a warning. *)
  D;
  () = EOF;
    { a + b + c + d }
