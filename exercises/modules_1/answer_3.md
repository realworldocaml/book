1.
1.
  
  One solution is to add explicit type constraints to the source
  program.  For example, if we revise the definition
  for `add` as follows, the correct type for it will be
  inferred.
```ocaml
  let add x (s : 'a set) : 'a set = x :: s
```
  
1. The inferred type for this program is the following.
  
```ocaml
  val cell : '_a list ref
  val push : '_a -> unit
  val pop : unit -> '_a
```
  Syntactically, the problem with this signature is the type
  variable `'_a`, which is not allowed in an interface
  file.  The real problem is that the type of the reference cell is
  unspecified, but it can only be used with one type.  To fix the
  signature, we must choose a specific type for the stack.  For example,
  the following is a valid signature that specifies that the stack is a
  stack of integers.
  
```ocaml
  val push : int -> unit
  val pop : unit -> int
```

