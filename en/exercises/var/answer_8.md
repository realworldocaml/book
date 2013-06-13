1.
  Answer.
```ocaml
  let (+:) s c = (fun i -> s i + c)
```

1.
  Answer.
```ocaml
  let (-|) s1 s2 = (fun i -> s1 i - s2 i)
```

1.
  Answer.
```ocaml
  let map f s = (fun i -> f (s i))
```

1.
  Answer.
```ocaml
  let integral s i =
     let rec loop sum j =
        if j = i then
           sum
        else
           loop (sum + s j) (j + 1)
     in
        loop 0
```

