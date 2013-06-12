  
## Exercise
  The input functions raise the `End_of_file` exception
  when the end of file is reached, which dictates a style where input
  functions are always enclosed in exception handlers.  The following
  function is not tail-recursive (see
  Section~\reflabelsection{tail-recursion}), which means the stack may
  overflow if the file is big.
  
```ocaml
  let read_lines chan =
     let rec loop lines =
        try loop (input_line chan :: lines) with
           End_of_file -> List.rev lines
     in
     loop []
```
1. Why isn't the function \lstinline$read_lines$ tail-recursive?
1. How can it be fixed?
  
