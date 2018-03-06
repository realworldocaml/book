  $ ocamlc -i -principal non_principal.ml
  File "non_principal.ml", line 6, characters 4-7:
  Warning 18: this type-based field disambiguation is not principal.
  type s = { foo : int; bar : unit; }
  type t = { foo : int; }
  val f : s -> int
