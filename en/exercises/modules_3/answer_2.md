1.
  The first three expressions are legal because, in each of
  the \lstinline$D$ modules, the type \lstinline$t$
  is \lstinline$int$.  For the remaining four expressions, the
  functor \lstinline$G$ produces a module with
  signature \lstinline$T$, where the type \lstinline$t$ is
  abstract.  In part 6, the expressions have types
  \lstinline$E1.x : G(A).t$ and \lstinline$E3.x : G(C).x$.
  The types \lstinline$G(A).t$ and $G(C).t$ are different, even
  though modules \lstinline$A$ and \lstinline$C$ are
  equal.

