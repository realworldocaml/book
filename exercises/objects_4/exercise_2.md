  
  
## Exercise
  Write an imperative version of a polymorphic map.  A newly-created map
  should be empty.  The class should have the following type.
  
```ocaml
  class ['a, 'b] imp_map : ('a -> 'a -> ordering) ->
    object
      method find   : 'a -> 'b
      method insert : 'a -> 'b -> unit
    end
```
  
