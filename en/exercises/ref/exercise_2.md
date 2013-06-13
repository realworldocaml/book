  
## Exercise
  \label{keyword:lazy}
  \index{lazy value}
  A \emph{lazy} value is a computation that is deferred until it is needed; we say that it is
  \emph{forced}.  A forced value is memoized, so that subsequent forcings do not reevaluate the
  computation.  The OCaml standard library already provides an implementation of lazy values in the
  `Lazy` module, but we can also construct them ourselves using reference cells and
  functions.
  
```ocaml
  type 'a deferred
  val defer : (unit -> 'a) -> 'a deferred
  val force : 'a deferred -> 'a
```
  Implement the type `'a deferred` and the functions `defer` and `force`.
  
