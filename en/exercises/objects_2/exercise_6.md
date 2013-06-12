  
## Exercise
  The following type definition uses polymorphic variants to specify an open type for simple
  arithmetic expressions with variables.
  
```ocaml
  type 'a exp =
   [> `Int of int
    | `Var of string
    | `Add of 'a exp * 'a exp
    | `Sub of 'a exp * 'a exp ] as 'a
```
1.
  Build an object-oriented version of expressions, where class type `exp` includes an
  evaluator that computes the value of the expression.
  
```ocaml
  class type env =
  object ('self)
     method add : string -> int -> 'self
     method find : string -> int
  end
  
  class type exp =
  object
     method eval : 'a. (#env as 'a) -> int
  end
```
  The classes should have the following types.
  
```ocaml
  class int_exp : int -> exp
  class var_exp : string -> exp
  class add_exp : #exp -> #exp -> exp
  class sub_exp : #exp -> #exp -> exp
```
  
  
1.
  
  Implement a new kind of expression ``Let of string * exp * exp`,
  where ``Let (v, e1, e2)` represents a let-expression
  `let v = e1 in e2`.
  
  
1.
  
  Suppose that, in addition to being able to evaluate an expression, we wish to check whether it
  is \emph{closed}, meaning that it has no undefined variables.  For the polymorphic variant form, the
  definition can be expressed concisely.
  
```ocaml
  let rec closed defined_vars = function
     `Int _ -> true
   | `Var v -> List.mem v defined_vars
   | `Add (e1, e2)
   | `Sub (e1, e2) -> closed defined_vars e1 && closed defined_vars e2
   | `Let (v, e1, e2) ->
       closed defined_vars e1 && closed (v :: defined_vars) e2
```
  Implement a method `closed : bool` for the expression classes.
  Any new classes should be defined by inheriting from the existing ones.
  How many new classes need to be defined?
  
