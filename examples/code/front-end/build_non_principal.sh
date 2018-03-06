  $ ocamlc -i -principal non_principal.ml
  [1mFile "[1mnon_principal.ml", line 6, characters 4-7[0m[0m:
  [1;35mWarning[0m 18: this type-based field disambiguation is not principal.
  type s = { foo : int; bar : unit; }
  type t = { foo : int; }
  val f : s -> int
