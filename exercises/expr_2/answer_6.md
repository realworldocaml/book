1.
  The problem is the value restriction.  The expression `f 0` has type
  `'_a -> '_a`, so it is not properly polymorphic.  The eta-expansion is not a good solution either,
  because it does not optimize the computation of `g 0`.
```ocaml
  let f x y = h (g x) y
```
  
  The only real solution is to ``lift'' the computation `g x` out of the function `f`,
  so that it may be computed explicitly.
  
```ocaml
  let z = g 0
  let v1 = h z "abc"
  let v2 = h z 1.2
```
  However, this may not always be desirable because it exposed the implementation of the function
  `f`.

