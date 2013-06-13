  
## Exercise
  In Exercise~\ref{exercise:dict1} we defined the data structure called a \emph{dictionary}.  Another
  way to implement a dictionary is with tree, where each node in the tree has a label \emph{and} a
  value.  Implement a polymorphic dictionary, `('key, 'value) dictionary`, as a tree with
  the three dictionary operations.
  
```ocaml
  empty : ('key, 'value) dictionary
  add   : ('key, 'value) dictionary -> 'key -> 'value -> ('key, 'value) dictionary
  find  : ('key, 'value) dictionary -> 'key -> 'value
```
  
