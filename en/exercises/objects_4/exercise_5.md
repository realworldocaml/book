  
  
## Exercise
  In the implementations for the \lstinline$['a] node$
  and \lstinline$['a] leaf$ classes in
  Section~\reflabelsection{polymorphic-class-types}, the
  function \lstinline$compare$ is threaded through the class
  definitions.  Implement a functor \lstinline$MakeTree$,
  specified as follows.
  
```ocaml
  type ordering = Smaller | Equal | Larger
  
  module type CompareSig = sig
     type t
     val compare : t -> t -> ordering
  end;;
  
  class type ['a] tree =
    object ('self)
      method add : 'a -> 'a tree
      method mem : 'a -> bool
    end;;
  
  module MakeTree (Compare : CompareSig)
    : sig val empty : Compare.t tree end =
  struct $\cdots$ end
```
  
