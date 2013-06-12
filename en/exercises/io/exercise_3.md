  
## Exercise
  Exceptions can have adverse interactions with input/output.  In
  particular, unexpected exceptions may lead to situations where files
  are not closed.  This isn't just bad style, on systems where the
  number of open files is limited, this may lead to program failure.
  Write a function
  \lstinline+with_in_file : string -> (in_channel -> 'a) -> 'a+
  to handle this problem.  When the
  expression \lstinline+with_in_file filename f+ is evaluated,
  the file with the given \lstinline+filename+ should be opened,
  and the function \lstinline+f+ called with the
  resulting \lstinline+in_channel+.  The channel should be closed
  when \lstinline+f+ completes, even if it raises an exception.
  
