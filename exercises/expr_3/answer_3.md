1.
  The comparison can be implemented as a pattern matching on the pair of numbers to be compared.
  
```ocaml
  let lt_small i j =
     match i, j with
        Zero, (One | Two | Three)
      | One, (Two | Three)
      | Two, Three ->
           true
      | _ ->
           false
```
  However, this implementation has poor style because of the wildcard matching `_ -> false`.
  If another number, like `Five`, is added to the type, the implementation of
  `lt_small` must be changed.  It is better to avoid the use of wildcards.
  
```ocaml
  let lt_small i j =
     match i, j with
        Zero, (One | Two | Three)
      | One, (Two | Three)
      | Two, Three ->
           true
      | Zero, Zero
      | One, (Zero | One)
      | Two, (Zero | One | Two)
      | Three, (Zero | One | Two | Three) ->
           false
```

1.
  With the implementation style above, the code is quadratic $O(n^2)$.
  
  One way to reduce the code size in this case is to map the type onto a linear subrange of the
  integers, then use the builtin comparison.
  
```ocaml
  let index_of_small = function
     Four -> 4
   | Three -> 3
   | Two -> 2
   | One -> 1
  
  let lt_small i j = index_of_small i < index_of_small j
```

