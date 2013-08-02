1.
  This is a non-tail-recursive version of `map`.
  
```ocaml
  let rec map f = function
     Nil -> Nil
   | Cons (h, t) -> Cons (f h, map f t)
```
  For a tail-recursive version, we collect the list in reverse order.
  
```ocaml
  let rec rev accum = function
     Nil -> accum
   | Cons (h, t) -> rev (Cons (h, accum)) t
  
  let map f l =
     let rec loop accum = function
        Nil -> rev Nil accum
      | Cons (h, t) -> loop (Cons (f h, accum)) t
     in
        loop Nil l
```

1.
  We give the tail-recursive version, using the function `rev` as defined above.
```ocaml
  let append l1 l2 =
     let rec loop l2 = function
        Cons (h, t) -> loop (Cons (h, l2)) t
      | Nil -> l2
     in
        loop l2 (rev l1)
```

