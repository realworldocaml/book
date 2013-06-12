1.
  In the first version, the function `f` is called within the exception handler, which means that
  if `f` raises the `Not_found` exception, then `callf` will return 0, the same as if
  the `List.assoc` function raises `Not_found`.
  
  The second version separates the calls.  If the function `f` raises `Not_found`, the
  exception will be propagated through the calls to `callf`.  The second form is preferable in
  situations where exceptions raised from `f` indicate an error, not normal operation.

