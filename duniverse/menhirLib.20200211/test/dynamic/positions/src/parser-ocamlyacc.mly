%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token DOT COMMA

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%type<Aux.annotations> annotations
%type<Aux.main> main
%start main

/* For now, we do not use [Parsing.symbol_start_pos()] because it performs
   a computation that Menhir does not (yet) emulate. In order to get the
   "real" startpos, the one that is stored in the stack, we use
   [Parsing.symbol_start_pos()] for epsilon productions and
   [Parsing.rhs_start_pos 1] for non-epsilon productions. */

%{ open Aux %}

%%

main:
| nothing expr EOL
    { (Parsing.rhs_start_pos 1, Parsing.symbol_end_pos()), $1, $2, Parsing.symbol_start_pos() }

/* Added just to exercise productions with an empty right-hand side. */
nothing:
| /* nothing */
    { (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()) }

/* Added just to exercise productions with an empty right-hand side, in a choice. */
optional_dot:
| nothing
    { (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()), Some $1 }
| DOT
    { (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()), None }

optional_comma:
| nothing
    { (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()), Some $1 }
| COMMA
    { (Parsing.symbol_start_pos(), Parsing.symbol_end_pos()), None }

annotations:
  optional_dot optional_comma
    { (Parsing.rhs_start_pos 1, Parsing.symbol_end_pos()),
      Parsing.rhs_end_pos 1, Parsing.rhs_start_pos 2, Parsing.rhs_start 1, Parsing.symbol_start_pos(),
      $1, $2 }

raw_expr:
| INT
    { EInt }
| annotations LPAREN nothing expr RPAREN optional_dot
    { EParen($1, $3, $4, $6, Parsing.symbol_start()) }
| expr PLUS expr
    { EBinOp ($1, $3) }
| expr MINUS expr
    { EBinOp ($1, $3) }
| expr TIMES expr
    { EBinOp ($1, $3) }
| expr DIV expr
    { EBinOp ($1, $3) }
| MINUS expr %prec UMINUS
    { EUnOp $2 }

expr:
  raw_expr
    { (Parsing.rhs_start_pos 1, Parsing.symbol_end_pos()), Parsing.rhs_end_pos 0, Parsing.rhs_end 0, $1 }

