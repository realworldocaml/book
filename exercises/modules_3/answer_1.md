1.
1.
  
  Legal; the functor \lstinline$F$ is the identity functor.
1.
  
  Legal; the module expression \lstinline$Apply (F) (A)$ is
  equivalent to \lstinline$F (A)$.
1.
  
  Not legal; the module expression
  \lstinline$F(struct let i = 1 end)$
  is not an expression.
1.
  
  Not legal; the module parameter \lstinline$X$ must have a
  signature (as in $F (X : XSig)$).
1.
  
  Legal; the module \lstinline$X$ has the specified
  signature \lstinline$sig type t = A | B end$.
1.
  
  Legal; the structure \lstinline$struct type t = A | B end$ has
  signature \lstinline$sig type t = A | B end$.
1.
  
  Not legal; the types \lstinline$X.t$ and
  \lstinline$type t = X.t$
  are different types.

