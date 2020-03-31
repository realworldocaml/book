%start<unit> phrase
%token A B EOF

%%

%inline apply(f, x):
  f(x) { $1 }

%inline apply_to_quux(f):
  f(quux) { $1 }
  (* This is a reference to [quux], which is not defined here. It is defined
     as *private* in the file join-param-2.mly, so Menhir should complain that
     [quux] is undefined. This was an obscure bug until 2016/05/18: this
     reference was ignored (because it appears as a parameter to a
     locally-bound variable!) and the code was accepted. *)

foo:
  apply(list, A) {}

bar:
  apply_to_quux(list) {}

phrase:
  foo bar EOF {}
