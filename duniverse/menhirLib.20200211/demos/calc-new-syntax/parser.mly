%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%start <int> main

(* In this demo, we do not use precedence declarations (%left, etc.).
   Instead, we manually stratify the grammar. It is quite easy and
   allows us to demonstrate the use of parameterized definitions. *)

%%

(* -------------------------------------------------------------------------- *)

(* [fold_left(op, elem)] recognizes a nonempty, left-associative list of
   elements, separated with operators. The semantic value of the symbol [op]
   is expected to be a binary function, which is applied to the left-hand
   summary and to the right-hand element. *)

let fold_left(op, elem) :=
  | elem
  | sum = fold_left(op, elem); ~ = op; ~ = elem; { op sum elem }

(* -------------------------------------------------------------------------- *)

(* [app(f, x)] recognizes the sequence [f; x]. Its semantic value is the
   application of the semantic value of [f] to the semantic value of [x]. *)

let app(f, x) ==
  ~ = f; ~ = x; { f x }

(* -------------------------------------------------------------------------- *)

(* We wish to parse an expression followed with an end-of-line. *)

(* The notation <> is a short-hand for a semantic action {...} that builds
   a tuple of the variables that have been introduced. Here, one variable
   [expr] has been introduced by [~ = expr], so <> stands for {expr}. *)

let main :=
  ~ = expr; EOL; <>

(* An expression is an additive expression. *)

let expr ==
  additive_expr

(* An additive expression is a left-associative list of multiplicative
   expressions, separated with additive operators. *)

let additive_expr ==
  fold_left(additive_op, multiplicative_expr)

(* These are the additive operators and their meaning. *)

let additive_op ==
  | PLUS;  { ( + ) }
  | MINUS; { ( - ) }

(* A multiplicative expression is a left-associative list of atomic
   expressions, separated with multiplicative operators. *)

let multiplicative_expr ==
  fold_left(multiplicative_op, atomic_expr)

(* These are the multiplicative operators and their meaning. *)

let multiplicative_op ==
  | TIMES; { ( * ) }
  | DIV;   { ( / ) }

(* An atomic expression is one of:
   an integer literal,
   an expression between parentheses,
   an application of a unary operator to an atomic expression. *)

let atomic_expr :=
  | INT
  | delimited(LPAREN, expr, RPAREN)
  | app(unary_op, atomic_expr)

(* These are the unary operators and their meaning. *)

let unary_op ==
  | MINUS; { (~- ) }
