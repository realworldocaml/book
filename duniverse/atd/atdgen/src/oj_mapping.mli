(** OCaml-Json decorated ATD AST. *)

type t = (Ocaml.Repr.t, Atd.Json.json_repr) Mapping.mapping
type variant_mapping =
  (Ocaml.Repr.t, Atd.Json.json_repr) Mapping.variant_mapping

val defs_of_atd_modules
  : ('a * Atd.Ast.module_body) list
  -> target:Ocaml.target
  -> ('a * (Ocaml.Repr.t, Atd.Json.json_repr) Mapping.def list) list
