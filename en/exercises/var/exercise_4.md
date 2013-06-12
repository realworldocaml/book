  
## Exercise
  Euclid's algorithm computes the greatest common divisor (GCD) of two integers.  It is one of the
  oldest known algorithms, appearing in Euclid's \emph{Elements} in roughly 300 BC.  It can be defined in
  pseudo-code as follows, where $\leftarrow$ represents assignment.
  
```ocaml
  gcd($n$, $m$) =
     while $m \neq 0$
        if $n > m$
           $n \leftarrow n - m$
        else
           $m \leftarrow m - n$
     return $n$
```
  Write an OCaml function
  `%%`
  that computes the GCD using Euclid's algorithm (so 
  \hbox{`n %% m`}
  is the GCD of the integers `n` and `m`).  You should define it without
  assignment, as a recursive function.  [Note, this is Euclid's original definition of the
    algorithm.  More modern versions usually use a modulus operation instead of subtraction.]
  
