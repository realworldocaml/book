  
## Exercise
  A \emph{stream} is an infinite sequence of values supporting an operation `hd($s$)` that
  returns the first value in the stream $s$, and `tl($s$)` that returns a new stream with
  the first element removed.
  
  One way to implement a stream is to represent it as a function over the nonnegative integers.  Given
  a stream `s : int -> int`, the first element is `(s 0)`, the second
  is `(s 1)`, \emph{etc}.  The operations are defined as follows.
  
```ocaml
  let hd s = s 0
  let tl s = (fun i -> s (i + 1))
```
  For this exercise, we'll assume that we're working with streams of integers, so the
  type `stream` is `int -> int`.  We'll write a stream as a sequence $(x_0, x_1,
  x_2, \ldots)$.
  
1. Define the following stream functions.
  \begin{itemize}
1. `(+:) : stream -> int -> stream`. Add a constant to a stream.
  
  `$(x_0, x_1, x_2, \ldots)$ +: $c$ = $(x_0 + c, x_1 + c, x_2 + c, \ldots)$`.
  
  
1. `(-|) : stream -> stream -> stream`.
  
  Subtract two streams pointwise.
  
  `$(x_0, x_1, x_2, \ldots)$ -| $(y_0, y_1, y_2, \ldots)$ = $(x_0 - y_0, x_1 - y_1, x_2 - y_2. \ldots)$`.
  
  
1. `map : (int -> int) -> stream -> stream`.
  
  Apply a function to each element of the stream.
  
  `map $f$ $(x_0, x_1, x_2, \ldots)$ = $(f\ x_0, f\ x_1, f\ x_2, \ldots)$`.
  
  
  \end{itemize}
  
1. A ``derivative'' function can be defined as follows.
```ocaml
  let derivative s = tl s -| s
```
  Define a function `integral : stream -> stream` such that, for any stream $s$,
  `integral (derivative $s$) = $s$ +: $c$` for some constant $c$.
  
