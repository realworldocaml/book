1.
1. 1
  
1.
  
  When `g` is called, `f` is called with the argument `100`, raising the
  exception `A 1`, passing control back to `g` which returns `1`.
  
1.
  
  The expression `g i` returns 1 for any value $\texttt{i} \ge 0$.  The function `f` always
  raises the exception `A 0`, which passes control to the \emph{innermost} exception handler for
  `g`, which then returns 1.  As the call stack unwinds, the return value `1` is passed unchanged.
  

