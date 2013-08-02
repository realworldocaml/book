1.
  The order of the let-expressions and initializers follows the textual order.
  Class `a` is nested within `b`, but before `b`'s initializer.
  The sequence is the following.
  
```ocaml
  B let
  A let
  A init
  B init
```

