1.
1.
  
  The function \lstinline$loop$ is not tail-recursive because the
  recursive call is enclosed in the \lstinline$try$ block.
  
1.
  
  One solution that keeps the general style is to introduce
  a function \lstinline$maybe_input_line$ that produces a string
  option instead of raising an exception.
  
```ocaml
  let maybe_input_line chan =
     try Some (input_line chan) with
        End_of_file -> None
  
  let read_lines chan =
     let rec loop lines =
        match maybe_input_line chan with
           Some line -> loop (line :: lines)
         | None -> List.rev lines
     in
     loop []
```

