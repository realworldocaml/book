1.
  The main problem with writing the `input_lines` function is in catching the `End_of_file` exception.
  The following program is inefficient, because the exception stack is linear in the length of the input file.
  For large files, the stack will likely overflow.
  
  \begin{center}
```ocaml
  let rec input_lines stdin =
      try input_line stdin :: input_lines stdin with
         End_of_file ->
            []
```
  \end{center}
  The way to code this efficiently is to wrap the `input_line` function to catch the exception.
  
  \begin{center}
```ocaml
  let maybe_input_line stdin =
     try Some (input_line stdin) with
        End_of_file ->
           None
  
  let input_lines stdin =
     let rec input lines =
        match maybe_input_line stdin with
           Some line -> input (line :: lines)
         | None -> List.rev lines
     in
        input []
```
  \end{center}

