(* A group of several files, where each file defines token aliases
   that are used in other files. *)

%token LPAREN "("
%token RPAREN ")"
%token<int> INT

%%

%public atom:
  "(" term ")"
    { $2 }
| INT
    { $1 }
