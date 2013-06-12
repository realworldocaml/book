1.
  The second form is preferable.  The first version is not tail-recursive, and the depth of the
  exception stack is linear in the number of entries in the \lstinline+names+ list.  The second version
  does not have these problems; it is properly tail-recursive.

