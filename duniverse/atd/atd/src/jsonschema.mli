(**
   Translate an ATD file to JSON Schema, honoring the <json ...> annotations.
*)

(** This is for validating the ATD file, not for JSON Schema. *)
val annot_schema : Annot.schema

(** Translate an ATD AST to a JSON Schema. *)
val print :
  src_name:string ->
  root_type:string ->
  out_channel -> Ast.full_module -> unit
