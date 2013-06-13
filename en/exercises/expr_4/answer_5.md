1.
```ocaml
  let string_reverse s =
     let len = String.length s in
     for i = 0 to len / 2 - 1 do
        let c = s.[i] in
        s.[i] <- s.[len - i - 1];
        s.[len - i - 1] <- c
     done
```

