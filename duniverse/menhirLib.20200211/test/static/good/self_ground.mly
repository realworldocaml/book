(* Because [self] ignores its argument (it calls itself recursively
   with a ground actual argument), expansion terminates. This grammar
   can (and should) be accepted. *)

%token A B C
%start<unit> start
%%

self(X):
  A self(B) {}
| C {}

start:
  self(A) {}
