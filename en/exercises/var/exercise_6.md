  
## Exercise
  A \emph{dictionary} is a data structure that represents a map from keys to values.  A dictionary has
  three operations.
  
  \begin{itemize}
1. `empty : dictionary`
1. `add   : dictionary -> key -> value -> dictionary`
1. `find  : dictionary -> key -> value`
  \end{itemize}
  The value `empty` is an empty dictionary; the expression `add dict key value`
  takes an existing dictionary `dict` and augments it with a new binding
  `key -> value`; and the expression `find dict key` fetches the value in the
  dictionary `dict` associated with the `key`.
  
  One way to implement a dictionary is to represent it as a function from keys to values.  Let's
  assume we are building a dictionary where the key type is `string`, the value type is
  `int`, and the empty dictionary maps every key to zero.  This dictionary can be implemented
  abstractly as follows, where we write $\mapsto$ for the map from keys to values.
  
  \begin{eqnarray*}
  \ms{empty} & = & \ms{key} \mapsto 0\\
  \ms{add}(\ms{dict}, \ms{key}, v) & = & \ms{key}' \mapsto \cases{v & if $\ms{key}' = \ms{key}$\cr
  \ms{dict}(\ms{key}) & otherwise}\\
  \ms{find}(\ms{dict}, \ms{key}) & = & \ms{dict}(\ms{key})
  \end{eqnarray*}
  
1. Implement the dictionary in OCaml.
  
  
1.
  Suppose we have constructed several dictionaries as follows.
  
```ocaml
  let dict1 = add empty "x" 1
  let dict2 = add dict1 "y" 2
  let dict3 = add dict2 "x" 3
  let dict4 = add dict3 "y" 4
```
  
  What are the values associated with `"x"` and `"y"` in each of the four dictionaries?
  
