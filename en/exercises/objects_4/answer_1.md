1.
1.
  
  Legal; the type is \lstinline$class c1 : object val x : 'a list end$
  
1.
  
  Legal; the type produced by the toploop is
  
```ocaml
  class c2 : object val x : 'a list ref end
```
  This typing seems a little strange, since \lstinline$x$ is not truly
  polymorphic (its type should be \lstinline$'_a list ref$).
  
1.
  
  Legal; the type is \lstinline$class c3 : 'a -> object val y : 'a end$.
  
1.
  
  Not legal; the method \lstinline$z$ has a polymorphic
  type \lstinline$'a$, which is not a parameter of the class definition.
  
1.
  
  Legal; the type is
  \lstinline$class c5 : int -> object val y : int method z : int end$.
  
1.
  
  Legal; the constraint means the class type is not polymorphic.  The type
  is \lstinline$class c6 : int -> object method y : int end$.

