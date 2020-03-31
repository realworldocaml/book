(* A group of several files, where each file defines token aliases
   that are used in other files. *)

%token EOL
%start<unit> phrase

%%

phrase:
  term EOL
    { print_endline $1 }
