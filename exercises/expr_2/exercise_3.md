  
## Exercise
  One of the issues with tuples is that there is no general destructor function that takes a tuple and
  projects an element of it.  Suppose we try to write one for triples.
  
```ocaml
  let nth i (x, y, z) =
     match i with
        1 -> x
      | 2 -> y
      | 3 -> z
      | _ -> raise (Invalid_argument "nth")
```
  
1. What is the type of the `nth` function?
  
  
1. Is there a way to rewrite the function so that it allows the elements of the tuple to have different types?
  
