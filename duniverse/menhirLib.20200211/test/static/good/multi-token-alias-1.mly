(* A group of several files, where each file defines token aliases
   that are used in other files. *)

%token MUL "*"
%token DIV "/"

%%

%public factor:
  factor "*" atom
    { $1 * $3 }
| factor "/" atom
    { $1 / $3 }
| atom
    { $1 }
