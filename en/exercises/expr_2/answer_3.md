1.
  The type is `nth : int -> 'a * 'a * 'a -> 'a`

1.
  If the elements of the tuple had different types, the function `nth` would return values
  of different types for different argument `i`.  This is not expressible in the type system we
  have seen.  Another argument is that, since the branches of a conditional (like `match`)
  must have the same type, the elements of the tuple must have the same type.

