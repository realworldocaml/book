  
  
## Exercise
  In the object implementation of the evaluator in
  Figure~\reffigure{implementing-evaluator}, the method \lstinline$eval$
  takes an environment of exact type \lstinline$int env$.  Suppose we
  try to change it to the following definition.
  
```ocaml
  class type exp =
    object ('self)
      method eval : int #env -> int
    end
  
  class int_exp (i : int) : exp =
    object (self : 'self)
      method eval (_ : int #env) = i
    end;;
  $\cdots$
```
1.
  
  The new type definition is accepted, but the class
  definition \lstinline$int_exp$ is rejected.  How can it be fixed?
  
1. Are there any advantages to the new definition?
  
