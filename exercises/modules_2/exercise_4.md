  
## Exercise
  In OCaml, signatures can apparently contain multiple declarations for the same value.
  
```ocaml
  # module type ASig = sig
     val f : 'a -> 'a
     val f : int -> int
    end;;
  @
  \begin{topoutput}
  module type ASig = sig val f : 'a -> 'a val f : int -> int end
  \end{topoutput}
  @
```
  In any structure that is given this signature, the
  function \lstinline$f$ must have \emph{all} the types listed.
  If \lstinline$f$ is not allowed to raise an exception, what is
  the only sensible definition for it?
  
