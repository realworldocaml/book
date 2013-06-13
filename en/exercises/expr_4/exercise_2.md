  
## Exercise
  Consider the following record type definition.
  
```ocaml
  type ('a, 'b) mpair = { mutable fst : 'a; snd : 'b }
```
  What are the types of the following expressions?
  
1. \lstinline$[|[]|]$
  
  
1. `{ fst = []; snd = [] }`
  
  
1. `{ { fst = (); snd = 2 } with fst = 1 }`
  
