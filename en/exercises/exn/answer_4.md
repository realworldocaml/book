1.
  In the first version, the function \lstinline+f+ is called within the exception handler, which means that
  if \lstinline+f+ raises the \lstinline+Not_found+ exception, then \lstinline+callf+ will return 0, the same as if
  the \lstinline+List.assoc+ function raises \lstinline+Not_found+.
  
  The second version separates the calls.  If the function \lstinline+f+ raises \lstinline+Not_found+, the
  exception will be propagated through the calls to \lstinline+callf+.  The second form is preferable in
  situations where exceptions raised from \lstinline+f+ indicate an error, not normal operation.

