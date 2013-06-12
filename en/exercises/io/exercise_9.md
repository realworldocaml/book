  
  
## Exercise
  Consider the following program.  The
  exception \lstinline+Scan_failure+ is raised when the input
  cannot be scanned because it doesn't match the format specification.
  
```ocaml
  try scanf "A%s" (fun s -> s) with
     Scan_failure _ ->
        scanf "B%s" (fun s -> s)
```
  What is the behavior of the this program when presented with the
  following input?
1. \lstinline+AA\n+
1. \lstinline+B\n+
1. \lstinline+AB\n+
1. \lstinline+C\n+
1. \lstinline+ABC\n+
  
