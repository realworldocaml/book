1.
1. 
  
  The definition is legal because the type variable \lstinline$'b$ is
  included as a part of \lstinline$'a$, so \lstinline$'b$ is not free in
  the definition.
  
1.
  
  Technically, all three annotations \lstinline$+'a$, \lstinline$-'a$,
  and \lstinline$'a$ are accepted.  However, since the
  type \lstinline$('b, 'b) pair$ is covariant in \lstinline$'b$, the
  definition is also covariant in \lstinline$'a$.
  
1.
  
  The type is \lstinline$(dog * cat) alt_animal_pair2$.  Note that the
  type \lstinline$dog * cat$ is artificial, it has nothing to do
  with whether the class represents a pair.
  
1.
  
  If the constraint is left out, the type variables \lstinline$'b$
  and \lstinline$'c$ become free in the class definition, so the
  definition is rejected.
  
1.
  
  The constraint \lstinline$constraint 'a = 'b -> 'c$ is also legal, but
  it means that the definition is no longer covariant in \lstinline$'a$.
  
1.
  
  Strictly speaking, it isn't necessary.  Suppose we have a class type
  `[-'a1, $\ldots$, -'an, +'b1, $\ldots$, +'bm] cl`.
  We can replace it with a single constraint `['c] cl`
  and the following constraint.
  
```ocaml
  constraint 'c = ('a1 * $\cdots$ * 'an) -> ('b1 * $\cdots$ * 'bn)
```
  We can't specify the variance of \lstinline$'c$, but the constraint
  enforces the variance.

