  
## Exercise
  Partial application is sometimes used to improve the performance of a multi-argument function when
  the function is to be called repeatedly with one or more of its arguments fixed.  Consider a
  function $f(x, y)$ that is to be called multiple times with $x$ fixed.  First, the function must be
  written in a form $f(x, y) = h(g(x), y)$ from some functions $g$ and $h$, where $g$ represents the
  part of the computation that uses only the value $x$.  We then write it in OCaml as follows.
  
```ocaml
  let f x =
     let z = $g(x)$ in
        fun y -> $h(z, y)$
```
  Calling $f$ on its first argument computes $g(x)$ and returns a function that uses the value
  (without re-computing it).
  
  Consider one root of a quadratic equation $a x^2 + b x + c = 0$ specified by the quadratic formula
  $\ms{r}(a, b, c) = {-b + \sqrt{b^2 - 4 a c} \over 2a}$.  Suppose we wish to to evaluate the
  quadratic formula for multiple values of $a$ with $b$ and $c$ fixed.  Write a function to compute
  the formula efficiently.
  
