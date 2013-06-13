1.
  The class `rectangle` adds a new dimension `h`.
```ocaml
  class rectangle x y w h =
  object
     inherit square
     method area = w * h
     method draw = Graphics.fill_rect x y w h
  end
```
  It is appropriate to say that the \emph{representation} of a rectangle includes the representation
  of a square.  The is-a relationships in the program are defined by the programmer.  They don't have
  to correspond to real-life relationships.

