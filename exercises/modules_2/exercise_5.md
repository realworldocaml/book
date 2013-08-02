  
## Exercise
  Unlike `val` declarations, `type`
  declarations must have distinct names in any structure or signature.
  
```ocaml
  # module type ASig = sig
       type t = int
       type t = bool
    end;;
  @
  \begin{toperror}
  Multiple definition of the type name t.
  Names must be unique in a given structure or signature.
  \end{toperror}
  @
```
  While this particular example may seem silly, the real problem is that
  all modules included with \lstinline$include$ must have disjoint type
  names.
  
```ocaml
  # module type XSig = sig
       type t
       val x : t
    end;;
  # module A : XSig = struct
       type t = int
       let x = 0
    end;;
  # module B : XSig = struct
       type t = int
       let x = 1
    end;;
  # module C = struct
       include A
       include B
    end;;
  @
  \begin{toperror}
  Multiple definition of the type name t.
  Names must be unique in a given structure or signature.
  \end{toperror}
  @
```
  Is this a problem?  If it is not, argue that conflicting includes
  should not be allowed in practice.  If it is, propose a possible
  solution to the problem (possibly by changing the language).
  
  
