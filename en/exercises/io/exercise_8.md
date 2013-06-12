  
## Exercise
  Suppose you are given a list of pairs of strings (of
  type `(string * string) list`.  Write a program to
  print out the pairs, separated by white space, in justified columns,
  where the width of the first column is equal to the width of the
  longest string in the column.  For example, given the
  input `[("a", "b"); ("ab", "cdef")]` the width of the
  first column would be 2.  Can you use `printf` to
  perform the formatting?
  
```ocaml
  # print_cols ["a", "b"; "abc", "def"];;
  @
  \begin{topoutput}
  a   b
  abc def
  \end{topoutput}
  @
```
  
