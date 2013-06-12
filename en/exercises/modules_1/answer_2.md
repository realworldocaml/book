1.
1.
  
  There are differences, but it isn't clear that one version is better
  than the other.  In version 1, the variable \lstinline+l+ is
  bound (and visible) in the \lstinline+rev_loop+ function.  Some
  typographical errors (for example, if we had
  written \lstinline+match l with ...+ instead
  of \lstinline+match l2 with ...+) will not be caught at compile
  time.
  
  Version 2 has a similar problem; the \lstinline+rev_loop+
  function is visible in the rest of the file, so it might be used
  accidentally.  Note however, that the
  signature \lstinline+rev.mli+ hides the definition from the
  rest of the program.
  
1.
  
  The partial application \lstinline+let rev = rev_loop []+ is
  not allowed, as it has type \lstinline+'_a -> '_a+,
  not \lstinline+'a -> 'a+.  See
  Section~\ref{section:value-restriction}, which discusses the value
  restriction.
  

