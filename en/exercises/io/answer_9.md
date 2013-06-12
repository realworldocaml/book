1.
1. The program returns the string \lstinline+"A"+.
1. The program returns the empty string.
1. The program returns the string \lstinline+"B"+.
1. The program raises the \lstinline+Scan_failure+ exception, removing
  the \lstinline+C+ from the input channel.
1.
  
  The program returns the string \lstinline+"C"+.  The
  first \lstinline+scanf+ fails, but removes
  the \lstinline+A+ from the input stream.  The
  second \lstinline+scanf+ matches the \lstinline+B+, and
  returns the string \lstinline+"C"+.

