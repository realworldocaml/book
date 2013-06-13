1.
1.
  
  The problem is that the type \lstinline$#env$ is polymorphic.
  We can fix the definition by using a polymorphic method type.
  
```ocaml
  class int_exp (i : int) : exp =
    object (self : 'self)
      method eval : 'a. (int #env as 'a) -> int = (fun _ -> i)
    end;;
```
  
1.
  
  There isn't really any reason to use the new definition, because 
  an environment of type \lstinline$int #env$ is no more useful than
  an environment of type \lstinline$int env$.  The only savings is
  in the outermost call to the evaluator, where is may be possible
  to omit a coercion.

