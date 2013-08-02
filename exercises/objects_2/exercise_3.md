  
## Exercise
  Normally, we would consider a square to be a subtype of rectangle.
  Consider the following class `square` that implements a square,
  
```ocaml
  class square x y w =
  object
     val x = x
     val y = y
     method area = w * w
     method draw = Graphics.fill_rect x y w w
     method move dx dy = {< x = x + dx; y = y + dy >}
  end
```
  Write a class `rectangle` that implements a rectangle by inheriting from `square`.
  Is it appropriate to say that a `rectangle` is a `square`?
  
