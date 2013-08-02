1.
  The simplest translation of the GCD function is as follows.
  
```ocaml
  let rec (%%) n m =
     if m = 0 then
        n
     else if n > m then
        (n - m) %% m
     else
        n %% (m - n)
```

