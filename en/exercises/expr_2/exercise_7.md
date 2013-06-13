  
## Exercise
  The function `append : 'a list -> 'a list -> 'a list` appends two lists.  It can be defined as follows.
  
```ocaml
  let rec append l1 l2 =
     match l1 with
        h :: t -> h :: append t l2
      | [] -> l2
```
  Write a tail-recursive version of `append`.
  
