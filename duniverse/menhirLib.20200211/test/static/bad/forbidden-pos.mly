%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%start <int> main

%%

let main :=
  | (i, j) = pair(INT, INT); EOL;
      { let _ = $startpos(j) in 42 }
