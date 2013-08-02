1.
  The type \lstinline$'a -> 'a$ is more general than the
  type \lstinline$int -> int$.  That is, any value with the
  former type can also be given the latter type; the inverse does not
  hold.  This means that the value must have
  \lstinline$'a -> 'a$.
  The only sensible value with this type is the identity function.
  
```ocaml
  module A : ASig = struct
     let f x = x
  end
```

1.

