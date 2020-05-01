(** Decorated ATD AST for OCaml validators. *)

type ov_mapping =
    (Ocaml.Repr.t, Validate.validate_repr) Mapping.mapping

val defs_of_atd_modules
  : ('a * Atd.Ast.module_body) list
  -> ('a * (Ocaml.Repr.t, Validate.validate_repr) Mapping.def list) list
