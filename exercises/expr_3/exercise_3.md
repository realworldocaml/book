  
## Exercise
  Suppose we have the following definition for a type of small numbers.
  
```ocaml
  type small = Four | Three | Two | One
```
  The builtin comparison `(<)` orders the numbers in reverse order.
  
```ocaml
  # Four < Three;;
  @
  \begin{topoutput}
  - : bool = true
  \end{topoutput}
  @
```
1.
  Write a function `lt_small : small -> small -> bool` that orders the numbers in the
  normal way.
  
  
1.
  
  Suppose the type `small` defines $n$ small integers.  How does the size of your code depend on $n$?
  
