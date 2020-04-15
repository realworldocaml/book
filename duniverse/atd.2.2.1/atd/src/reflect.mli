(**
  Conversion of an AST value into OCaml source code that creates this value
*)

val print_full_module_def : Buffer.t -> string -> Ast.full_module -> unit
  (**
     [print_full_module_def buf name x] prints OCaml source code
     that would construct the given ATD tree [x] and call it [name].
  *)
