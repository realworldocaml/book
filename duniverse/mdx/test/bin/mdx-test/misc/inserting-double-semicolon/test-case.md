`;;` is not mandatory at the end of toplevel phrases but will be inserted
automatically in the corrected file. Also, a warning will be emitted.

```ocaml
# let x = 3
val x : int = 3
# let f = function
  | 0 -> 1
  | n -> n-1
val f : int -> int = <fun>
```
