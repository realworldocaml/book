1.
  The principal change is to use pattern matching instead of the conditional.
  
```ocaml
  let rec insert compare x = function
     Leaf -> Node (x, Leaf, Leaf)
   | Node (y, left, right) as node ->
       match compare x y with
          LessThan -> Node (y, insert compare x left, right)
        | Equal -> node
        | GreaterThan -> Node (y, left, insert compare x right)
```

