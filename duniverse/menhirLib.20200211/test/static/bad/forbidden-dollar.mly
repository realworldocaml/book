%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%start <int> main

%%

let main :=
  | i = INT; EOL;
      { $2; i }
      (* The use of $2 is forbidden in the new rule syntax. *)
