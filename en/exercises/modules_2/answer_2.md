1.
  Recursive modules can be used for the top-down programming style.  The
  code is wrapped in a module, and all forward references must be
  declared in the module signature.  To illustrate, suppose we have a
  function \lstinline$main$ that calls
  functions \lstinline$f$ and \lstinline$g$, using an
  intermediate type \lstinline$t$.
  
```ocaml
  module rec Body : sig
     type t = V of int
     val main : int -> int
     val f : int -> t
     val g : t -> int
  end = struct
     open Body
  
     let main i = g (f i)
     let f i = V i
     let g (V i) = i
     type t = V of int
  end
```
  This approach has the usual disadvantage that types must be defined
  twice, once in the signature and once in the structure.  However, the
  type definition can be defined before the module.  This is common
  style even in top-down programming.

