  
## Exercise
  \index{modules!vs records@\textit{vs.}~records}
  It is sometimes better to define a data structure as a record instead
  of a module.  For example, the record type for the finite sets in this
  chapter might be defined as follows, where the
  type \lstinline$'elt t$ is the set representation for sets with
  elements of type \lstinline$'elt$.
  
```ocaml
  type 'elt t = $\cdots$
  type 'elt set =
     { empty : 'elt t;
       add   : 'elt -> 'elt t -> 'elt t;
       mem   : 'elt -> 'elt t -> bool;
       find  : 'elt -> 'elt t -> 'elt
     }
```
1.
  
  Write a function
  \lstinline$make_set : ('elt -> 'elt -> bool) -> 'elt set$
  that corresponds to the \lstinline$MakeSet$ functor on
  page~\pageref{page:mset1} (the argument to \lstinline$make_set$
  is the equality function).  Can you hide the definition of the type
  \lstinline$'elt t$
  from the rest of the program?
  
1.
  
  Is it possible to implement sets two different ways such that both
  implementations use the same \lstinline$'elt set$ type, but
  different \lstinline$'elt t$ representations?
  
1.
  
  Consider an alternative definition for sets, where the record type is
  also parameterized by the set representation.
  
```ocaml
  type ('elt, 't) set =
     { empty : 't;
       add   : 'elt -> 't -> 'elt;
       mem   : 'elt -> 't -> bool;
       find  : 'elt -> 't -> 'elt
     }
```
  Write the function \lstinline$make_set$ for this new type.  What is
  the type of the \lstinline$make_set$ function?
  
1.
  
  What are some advantages of using the record representation?  What are
  some advantages of using the functor representation?
  
