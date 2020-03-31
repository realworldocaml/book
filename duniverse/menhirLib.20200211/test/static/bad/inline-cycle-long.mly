%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%start <int> main

%%

let fold_left(op, elem) :=
  | elem
  | sum = fold_left(op, elem); ~ = op; ~ = elem; { op sum elem }

let app(f, x) ==
  ~ = f; ~ = x; { f x }

let main :=
  ~ = expr; EOL; <>

let expr ==
  additive_expr

let additive_expr ==
  fold_left(additive_op, multiplicative_expr)

let additive_op ==
  | PLUS;  { ( + ) }
  | MINUS; { ( - ) }

let multiplicative_expr ==
  fold_left(multiplicative_op, atomic_expr)

let multiplicative_op ==
  | TIMES; { ( * ) }
  | DIV;   { ( / ) }

let atomic_expr == (* this is the culprit *)
  | INT
  | delimited(LPAREN, expr, RPAREN)
  | delimited(LPAREN, app(unary_op, atomic_expr), RPAREN)

let unary_op ==
  | MINUS; { (~- ) }
