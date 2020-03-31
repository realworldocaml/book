(* In this demo, instead of interpreting arithmetic expressions on the fly
   as in the other demos (e.g. demos/calc), we build an AST, which we later
   interpret. *)

(* We use a two-level AST, where the types [expr] and [raw_expr] are mutually
   recursive; this means that every subexpression is annotated with its
   location in the input text. In this file, we use the parameterized symbol
   [located] to annotate semantic values with locations without polluting the
   grammar. *)

(* We make heavy use of ~ patterns and <...> semantic actions to avoid naming
   or numbering the semantic values that we manipulate. *)


%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%start <Syntax.expr> main
%{ open Syntax %}

%%

(* -------------------------------------------------------------------------- *)

(* We wish to parse an expression followed with an end-of-line. *)

let main :=
  ~ = expr; EOL; <>

(* An expression is an additive expression. *)

let expr ==
  additive_expr

(* An additive expression is

   either a multiplicative expression,
   or the application of an additive operator to two subexpressions.

   In the second case, the left-hand subexpression is additive, while the
   right-hand subexpression is multiplicative; this reflects the fact that
   the operator is left-associative. The three semantic values of interest
   (the left subexpression, the operator, the right subexpression) are
   matched against ~ patterns, which means that, in the end, the data
   constructor [EBinOp] is applied to a tuple of these three components.
   Furthermore, this whole construction is wrapped in [located], so the
   result of [EBinOp], a raw expression, is turned into an expression. *)

let additive_expr :=
  | multiplicative_expr
  | located(
      ~ = additive_expr; ~ = additive_op; ~ = multiplicative_expr; <EBinOp>
    )

(* These are the additive operators and their meaning. *)

let additive_op ==
  | PLUS;  { OpPlus }
  | MINUS; { OpMinus }

(* A multiplicative expression is either an atomic expression or the
   application of a multiplicative operator to two subexpressions. *)

let multiplicative_expr :=
  | atomic_expr
  | located(
      ~ = multiplicative_expr; ~ = multiplicative_op; ~ = atomic_expr; <EBinOp>
    )

(* These are the multiplicative operators and their meaning. *)

let multiplicative_op ==
  | TIMES; { OpTimes }
  | DIV;   { OpDiv }

(* An atomic expression is one of:
   an expression between parentheses,
   an integer literal,
   an application of a unary operator to an atomic expression. *)

(* Only the last two cases are wrapped in [located]; in the first case, this is
   not necessary, as the expression already carries a location. Note that, this
   way, we get tight locations (i.e., the parentheses are not included). *)

let atomic_expr :=
  | LPAREN; ~ = expr; RPAREN; <>
  | located(
    | ~ = INT; <ELiteral>
    | ~ = unary_op; ~ = atomic_expr; <EUnOp>
    )

(* These are the unary operators and their meaning. *)

let unary_op ==
  | MINUS; { OpNeg }

(* -------------------------------------------------------------------------- *)

(* [located(x)] recognizes the same input fragment as [x] and wraps its
   semantic value of type ['a] as a value of type ['a located]. *)

let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
