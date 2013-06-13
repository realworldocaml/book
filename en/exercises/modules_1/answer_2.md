1.
1.
  
  There are differences, but it isn't clear that one version is better
  than the other.  In version 1, the variable `l` is
  bound (and visible) in the `rev_loop` function.  Some
  typographical errors (for example, if we had
  written `match l with ...` instead
  of `match l2 with ...`) will not be caught at compile
  time.
  
  Version 2 has a similar problem; the `rev_loop`
  function is visible in the rest of the file, so it might be used
  accidentally.  Note however, that the
  signature `rev.mli` hides the definition from the
  rest of the program.
  
1.
  
  The partial application `let rev = rev_loop []` is
  not allowed, as it has type `'_a -> '_a`,
  not `'a -> 'a`.  See
  Section~\ref{section:value-restriction}, which discusses the value
  restriction.
  

