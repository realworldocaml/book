1.
  The problem is that the method `move` refers to the definitions of `get` and
  `set` when the rectangle is first created.  This is called \emph{early binding}, where
  true object systems use \emph{late} binding.  Early binding means that the method `move`
  is not updated when `set` is changed.
  
1.
  The expression `rect2.move (dx, dy)` moves `rect2` by `(dx, dy)`.  To
  prevent this from happening, the method `move` should be updated as well.
  
```ocaml
  let rect2 = { rect1 with set = (fun _ -> ()); move = (fun _ -> ()) }
```
  
1. The rectangles `rect2` and `rect1` refer to the same position `pos`,
  so setting the position of `rect1` also moves `rect2`.

