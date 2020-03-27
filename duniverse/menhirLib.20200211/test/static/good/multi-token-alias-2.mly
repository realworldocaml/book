(* A group of several files, where each file defines token aliases
   that are used in other files. *)

%token PLUS "+"
%token MINUS "-"

%%

%public term:
  term "+" factor
    { $1 + $3 }
| term "-" factor
    { $1 - $3 }
| factor
    { $1 }
