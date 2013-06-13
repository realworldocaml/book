1.
  Since the lists are sorted, we can search them from first to last, skipping a name when it doesn't
  match the other lists.  We'll use `l1`, `l2`, and `l3` for the lists.
  
```ocaml
  let rec find_crook l1 l2 l3 =
     match l1, l2, l3 with
        (h1 :: t1), (h2 :: t2), (h3 :: t3) ->
           if      h1 < h2 || h1 < h3 then find_crook t1 l2 l3
           else if h2 < h1 || h2 < h3 then find_crook l1 t2 l3
           else if h3 < h1 || h3 < h2 then find_crook l1 l2 t3
           else (* all three names h1,h2,h3 are the same *) h1
      | _ ->
           raise (Invalid_argument "no crooks")
```

