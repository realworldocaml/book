(** OCaml-Biniou decorated ATD AST. *)

type ob_mapping =
    (Ocaml.Repr.t, Biniou.biniou_repr) Mapping.mapping

val defs_of_atd_modules
  : ('a * Atd.Ast.module_body) list
  -> ('a * (Ocaml.Repr.t, Biniou.biniou_repr) Mapping.def list) list
