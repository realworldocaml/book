1.
  The easiest way to write this function is as a simple recursive definition.
  
```ocaml
  let rec sum n m f =
     if n > m then
        0
     else
        f n + sum (n + 1) m f
```
  This function is not tail-recursive.  A simple way to fix it is to define an auxiliary function
  `loop` that collects the intermediate result `x` in an
  \emph{accumulator}.
  
```ocaml
  let sum n m f =
     let rec loop i x =
        if i > m then
           x
        else
           loop (i + 1) (f i + x)
     in
        loop n
```

