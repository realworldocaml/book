  
## Exercise
  We can define a data type for simple arithmetic expressions as follows.
  
```ocaml
  type unop = Neg
  type binop = Add | Sub | Mul | Div
  type exp =
     Constant of int
   | Unary of unop * exp
   | Binary of exp * binop * exp
```
  Write a function `eval : exp -> int` to \emph{evaluate} an expression, performing the
  calculation to produce an integer result.
  
