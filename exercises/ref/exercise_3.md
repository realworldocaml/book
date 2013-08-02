  
## Exercise
  \index{lazy list}
  A lazy list is a list where the tail of the list is a deferred computation (a lazy list is also
  called a \emph{stream}).  The type can be defined as follows, where the type `deferred` is
  defined as in Exercise~\ref{exercise:lazy}.
  
```ocaml
  type 'a lazy_list =
     Nil
   | Cons of 'a * 'a lazy_list
   | LazyCons of 'a * 'a lazy_list deferred
```
  Define the following functions on lazy lists.
  
```ocaml
  val nil       : 'a lazy_list
  val cons      : 'a -> 'a lazy_list -> 'a lazy_list
  val lazy_cons : 'a -> (unit -> 'a lazy_list) -> 'a lazy_list
  val is_nil    : 'a lazy_list -> bool
  val head      : 'a lazy_list -> 'a
  val tail      : 'a lazy_list -> 'a lazy_list
  val (@@)      : 'a lazy_list -> 'a lazy_list -> 'a lazy_list
```
  The expression `$l_1$ @@ $l_2$` appends two lazy lists in constant time.
  
