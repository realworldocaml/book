  
## Exercise
  Consider the function `insert` for unbalanced, ordered, binary trees in
  Section~\refsection{unbalanced-btree}.  One potential problem with this implementation is that it
  uses the builtin comparison `(<)`.  Rewrite the definition so the it is parameterized by a
  comparison function that, given two elements, returns on of three values
  `type comparison = LessThan | Equal | GreaterThan`.
  The expression `insert compare x tree` inserts an element `x` into the tree
  `tree`.  The type is
  `insert : ('a -> 'a -> comparison) -> 'a -> 'a tree -> 'a tree`.
  
