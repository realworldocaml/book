%token<int> A B C D
%token EOF
%start<int> main

%%

let pair(x, y) ==
  ~ = x; ~ = y; { x, y }

let fst(p) :=
  (~, _) = p; <>

let snd(p) :=
  (_, ~) = p; <>

let main :=
  (a, b) = pair(A, B);
  c = fst(pair(A, B));
  d = snd(pair(A, B));
  _ = C;
  D;
  () = EOF;
    { a + b + c + d }
