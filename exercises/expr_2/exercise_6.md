  
## Exercise
  In Exercise~\ref{exercise:partial-application1} we saw that partial application is sometimes used to
  improve performance of a function $f(x, y)$ under the following conditions:
  \begin{itemize}
1. the function can be written in the form $f(x, y) = h(g(x), y)$, and
1. $f$ is to be called for multiple values of $y$ with $x$ fixed.
  \end{itemize}
  In this case, we code $f(x, y)$ as follows, so that $g(x)$ is computed when $f$ is partially
  applied to its first argument.
  
```ocaml
  let f x = h (g x)
```
  Unfortunately, this technique doesn't always work in the presence of polymorphism.
  Suppose the original type of the function is `f : int -> 'a -> 'a`,
  and we want to compute the values of `(f 0 "abc")` and `(f 0 1.2)`.
  
```ocaml
  let f' = f 0
  let v1 = f' "abc"
  let v2 = f' 1.2
```
  What goes wrong?  How can you compute both values without computing `g 0` twice?
  
