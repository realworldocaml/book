(** some utils related to the runtime of ocaml, used both at compile time (camlp4) and
    runtime. to be considered the same way than [Obj] (internal, unsafe, etc.) *)
val repr_of_poly_variant : [> ] -> int
val hash_variant : string -> int
val double_array_value : 'a
val has_double_array_tag : 'a -> bool
