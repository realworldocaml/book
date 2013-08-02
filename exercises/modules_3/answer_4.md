1.
1.
  
  The definition is a direct translation of the
  functor \lstinline$MakeSet$.
  
```ocaml
  type 'elt t = 'elt list
  let make_set equal =
     { empty = [];
       add = (::);
       mem = (fun x s -> List.mem (equal x) s);
       find = (fun x s -> List.find (equal x) s)
     }
```
  To keep the type \lstinline$'elt t$ abstract, we must enclose the
  definition in a module.
  
```ocaml
  module Set : sig
     type 'elt t
     type 'elt set = $\cdots$
     val make_set : ('elt -> 'elt -> bool) -> 'elt set
  end = struct
     type 'elt t = 'elt list
     type 'elt set = $\cdots$
     let make_set equal = $\cdots$
  end
```
  
1.
  
  No.  The type \lstinline$'elt t$ is fixed for all sets (of
  type \lstinline$'elt set$).
  
1.
  
  The implementation of the function \lstinline$make_set$ is
  unchanged.  It has the following type.
  
```ocaml
  type 'elt set_repr = 'elt list
  $\cdots$
  val make_set : ('elt -> 'elt -> bool) -> ('elt, 'elt set_repr) set
```
  The type \lstinline$set_repr$ is the representation for sets; the type
  definition can be hidden the usual way.
  
1.
  
  The main advantage of the record representation is that it
  is \emph{first class}, meaning that values of
  type \lstinline$'elt set$ can be passed as arguments, stored in
  data structures, etc.  There are several disadvantages.  Among them,
  type expressions are larger.  In the worst case, the type definition
  requires a parameter for each type that would be abstract in the
  module.  In addition, there is a slight performance penalty for
  calling a function in the \lstinline$set$ record; the functor
  does not have this penalty penalty because references are resolved at
  compile time.
  

