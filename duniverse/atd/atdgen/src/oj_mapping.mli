(** OCaml-Json decorated ATD AST. *)

type t = (Ocaml.Repr.t, Json.json_repr) Mapping.mapping
type variant_mapping = (Ocaml.Repr.t, Json.json_repr) Mapping.variant_mapping

val defs_of_atd_modules
  : ('a * Atd.Ast.module_body) list
  -> target:Ocaml.target
  -> ('a * (Ocaml.Repr.t, Json.json_repr) Mapping.def list) list

(** "A.B" -> "A.B.normalize" *)
val json_normalizer_of_adapter_path : string -> string

(** "A.B" -> "A.B.restore" *)
val json_restorer_of_adapter_path : string -> string
