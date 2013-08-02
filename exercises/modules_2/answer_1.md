1.
1.
  
  Legal; the value associated with a variable is specified by the last
  definition in the module.
1. 
  
  Legal; the duplicate type definition is legal.  What would happen if
  the type definitions differ?
1. 
  
  Not legal; the module name must begin with an uppercase letter.
1. 
  
  Legal; within the structure body the function `g` has
  type \lstinline$'a -> 'a$.  The type constraint is
  applied \emph{after} the structure is formed.
1. 
  
  Legal; the value is \lstinline$1$.
1. 
  
  Not legal; the function `h` must be defined before `g`.
1. 
  
  Legal; the forward reference to \lstinline$h x$ is allowed
  because the module is recursive.
1. 
  
  Legal; however a application of the function `f` will fail at
  runtime because the value cannot be resolved.
1. 
  
  Legal; `f` has type \lstinline$f : 'a -> 'a t$.
1.
  
  Not legal; OCaml produces the error message ``In this type, the
  locally bound module name M escapes its scope.''  This is because the
  the function `f` produces a value of type \lstinline$M.t$,
  but \lstinline$M$ is defined only in the body of `f`.
  
1.
  
  Legal; the modules `A` and `B` share a common (abstract) type,
  so it is legal to pass the result of \lstinline $A.f$
  t \lstinline$B.g$.
  
1.
  
  Legal; the \lstinline$include$ directive is like textual inclusion,
  subject to signature constraints.  After expansion, the module `C`
  has the following form.
  
```ocaml
  module C : sig
     type t
     val x : t
  end = struct
     type t = int (* sig type t = int *)
     let x = 1    (* sig val x : int *)
  end
```
  This is a legal module definition.

