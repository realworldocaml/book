  
## Exercise
  Suppose you are given a value of the following type, and you want to
  produce a string representation of the value.
  
```ocaml
  type exp =
     Int of int
   | Id of string
   | List of exp list
```
  The representation is as follows.
  \begin{itemize}
1. \lstinline+Int+ and \lstinline+Id+ values print as themselves.
1.
  
  \lstinline+List+
  values are enclosed in parentheses, and the elements in the list are
  separated by a single space character.
  \end{itemize}
  Write a function \lstinline$print_exp$ to produce the string representation for a value of
  type \lstinline+exp+.  The following gives an example.
  
```ocaml
  # print_exp (List [Int 2; Id "foo"]);;
  (2 foo)
```
  
