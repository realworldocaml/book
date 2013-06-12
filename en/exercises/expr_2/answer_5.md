1.
  There are only two kinds of functions with type `'a -> 'a`: the identity function (and all
  equivalent functions), and functions that does not terminate, for example
  `(fun x -> raise (Invalid_argument "error"))`.  One way to think about it is that the
  function takes an argument of some arbitrary type `'a`.  Since the actual type is not
  known, the function can do only one of two things: 1) it can return it without change, or 2) it can
  fail to terminate.

