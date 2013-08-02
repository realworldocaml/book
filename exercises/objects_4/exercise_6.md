  
  
## Exercise
  Instead of defining a class type \lstinline$class type ['a] tree$,
  we could have specified it as a virtual class like the following.
  
```ocaml
  class virtual ['a] virtual_tree =
    object (self : 'self)
      method virtual add : 'a -> 'a virtual_tree
      method virtual mem : 'a -> bool
    end;;
```
  Are there any advantages or disadvantages to this approach?
  
