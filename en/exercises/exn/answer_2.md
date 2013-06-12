1.
1. 1
  
1.
  
  When \lstinline+g+ is called, \lstinline+f+ is called with the argument \lstinline+100+, raising the
  exception \lstinline+A 1+, passing control back to \lstinline+g+ which returns \lstinline+1+.
  
1.
  
  The expression \lstinline+g i+ returns 1 for any value $\texttt{i} \ge 0$.  The function \lstinline+f+ always
  raises the exception \lstinline+A 0+, which passes control to the \emph{innermost} exception handler for
  \lstinline+g+, which then returns 1.  As the call stack unwinds, the return value \lstinline+1+ is passed unchanged.
  

