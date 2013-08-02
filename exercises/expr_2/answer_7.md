1.
  In contrast to the `map` function, which reverses the result, it is easier to reverse the
  argument for the `append` function.
  
```ocaml
  let append l1 l2 =
     let rec rev_append l2 = function
        x :: l1 -> rev_append (x :: l2) l1
      | [] -> l2
     in
        rev_append l2 (List.rev l1)
```

