%token FOO BAR
%start<unit> main
%attribute FOO(BAR) [@cost 0]
  (* The application FOO(BAR) is ill-sorted. *)

%%

seq(X, Y):
  x = X y = Y { (x, y) }

main:
  seq(FOO, BAR) { $1 }
