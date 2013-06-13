  
  
## Exercise
  In the class type definition \lstinline$['a] tree$ on
  page~\pageref{page:poly-tree}, the method \lstinline$add$ has
  type \lstinline$'a -> 'a tree$.  What would happen if we defined the
  class type as follows?
  
```ocaml
  class type ['a] self_tree =
    object ('self)
      method add : 'a -> 'self
      method mem : 'a -> bool
    end
```
  
