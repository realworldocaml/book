%token <string> NAME
%token LBRACKET RBRACKET EOF QUOTE DOT
%start main
%type <Types.sexp> main

%%

main:
  sexp   { $1 }
  ;

sexp:
  list { $1 }
| atom { $1 }
| QUOTE sexp { Types.Cons (Types.Atom "quote", Types.Cons($2, Types.Atom "nil")) }
;

list:
  LBRACKET RBRACKET { Types.Atom "nil" }
| LBRACKET inside_list RBRACKET { $2 }
;

inside_list:
  sexp DOT sexp { Types.Cons ($1,$3) }
| sexp { Types.Cons ($1, Types.Atom "nil") }
| sexp inside_list {Types.Cons($1,$2)}
;

atom: NAME { Types.Atom $1 }
;



