type unop =
  | OpNeg

type binop =
  | OpPlus | OpMinus | OpTimes | OpDiv

type 'a located =
  { loc: Lexing.position * Lexing.position; value: 'a }

type expr =
  raw_expr located

and raw_expr =
| ELiteral of int
| EUnOp of unop * expr
| EBinOp of expr * binop * expr
