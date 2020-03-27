%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%start <int> main

%%

let main =
  | i = INT; EOL;
      { $1 }
      (* The use of $1 is forbidden in the new rule syntax. *)
