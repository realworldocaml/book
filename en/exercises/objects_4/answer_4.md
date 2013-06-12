1.
  The alternate definition, using \lstinline$'self$ does not work because
  the class \lstinline$leaf$ must create a new internal node.  The expression
  \lstinline$new node x (self :> 'a tree) (self : 'a tree)$ has type
  \lstinline$'a tree$.  It doesn't have type \lstinline$'self$.

