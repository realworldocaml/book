  
## Exercise
  \index{memoization!of recursive functions}
  One problem with the memoization function `memo : ('a -> 'b) -> ('a -> 'b)` in
  Section~\ref{section:memoization} is that it ignores recursive definitions.  For example,
  the expression `memo fib $i$` still takes exponential time in $i$ to compute.
  
  To solve this, we'll need to modify the recursive definition for `fib` and perform an
  explicit memoization.  Implement the following types and functions, where
  `fib = memo_fib (create_memo ())`.  How fast is the Fibonacci function now?
  
```ocaml
  type ('a, 'b) memo
  val create_memo : unit -> ('a, 'b) memo
  val memo_find   : ('a, 'b) memo -> 'a -> 'b option
  val memo_add    : ('a, 'b) memo -> 'a -> 'b -> unit
  val memo_fib    : (int, int) memo -> int -> int
  
  let fib = memo_fib (create_memo ())
```
  
