1.
  Each function $f_i : int -> int$ would be defined in a separate class,
  where all the other functions are virtual.
  
```ocaml
  class virtual fun_$i$ =
  object
     method virtual $f_1$ : int -> int
     $\cdots$
     method virtual $f_{i - 1}$ : int -> int
     method $f_i$ i = $\cdots$
     $\cdots$
     method virtual $f_n$ : int -> int
  end
```
  The final class would use multiple inheritance to tie the recursive knot.
```ocaml
  class everything =
  object
     inherit fun_$1$ $\cdots$ inherit fun_$n$
  end
```
  To reduce the amount of code, a single shared base class could be used
  to declare all the functions.
  
```ocaml
  class virtual declarations =
  object
     method virtual $f_1$ : int -> int
     $\cdots$
     method virtual $f_n$ : int -> int
  end
  
  class virtual fun_$i$ =
  object
     inherit declarations
     method $f_i$ i = $\cdots$
  end
```
  An advantage of the class representation is that the text required
  for the declarations is \emph{much} smaller than needed for recursive
  modules.

