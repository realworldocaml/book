1.
  To add two numbers we use the following equivalences.
  
  \begin{eqnarray*}
  i + 0 & = & i\\
  i + (j + 1) & = & (i + 1) + j
  \end{eqnarray*}
  This gives us the following loop.
  
```ocaml
  let rec add i = function
     S j -> add (S i) j
   | Z -> i
```
  The time complexity of the expression `add $n$ $m$` is $O(m)$.

1.
  We can multiply two numbers by repeated summing.
  
```ocaml
  let mul n m =
     let rec loop sum = function
        Z -> sum
      | S m -> loop (add sum n) m
     in
        sum Z m
```

