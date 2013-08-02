1.
  Let's implement `new_scale` as a normal function.
  
```ocaml
  let new_scale sx sy =
     object
        val matrix = (sx, 0., 0., sy, 0., 0.)
        method transform (x, y) = $\cdots$
        method multiply matrix2 = $\cdots$
     end;;
```
  If the other functions were implemented the same way, the code for the methods `transform`
  and `multiply` would be duplicated.  We can avoid this by creating a generic constructor
  function that takes the entire matrix as an argument.
  
```ocaml
  let new_transform matrix =
     object
        val matrix = matrix
        method transform (x, y) = $\cdots$
        method multiply matrix2 = $\cdots$
     end
  
  let new_scale sx sy = new_matrix (sx, 0., 0., sy, 0., 0.)
  let new_translate dx dy = new_matrix (1., 0., dx, 0., 1., dy)
  let new_rotate theta =
     let s, c = sin theta, cos theta in
     new_transform (c, -.s, 0., s, c, 0.)
```

