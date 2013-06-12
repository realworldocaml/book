  
## Exercise
  Suppose we are given a \lstinline+table+ as in the last exercise, and we wish to call some function
  \lstinline+f+ on one of the entries, or returning 0 if the entry is not found.  That is, we are given the
  function \lstinline+f+, and a name, and we wish to evaluate \lstinline+f (List.assoc table name)+.  What is
  the difference between the following functions?
  
1. 
  \begin{center}
```ocaml
  let callf f name =
     try f (List.assoc table name) with
        Not_found ->
           0
```
  \end{center}
  
1.
  \begin{center}
```ocaml
  let callf f name =
     let i =
        try Some (List.assoc table name) with
           Not_found ->
              None
     in
        match i with
           Some j -> f j
         | None -> 0
```
  \end{center}
  
