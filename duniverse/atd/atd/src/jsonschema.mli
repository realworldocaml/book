(**
   Translate an ATD file to JSON Schema, honoring the <json ...> annotations.
*)

(** These are the versions of the JSON Schema currently supported.

    See https://json-schema.org/draft/2020-12/release-notes.html
    for the differences between 2019-09 and 2020-12.
*)
type version =
  | Draft_2019_09
  | Draft_2020_12

val default_version : version

(** This is for validating the ATD file, not for JSON Schema. *)
val annot_schema : Annot.schema

(** Translate an ATD AST to a JSON Schema.

    @param xprop whether to allow extra fields in JSON objects. The default
    is true, which is JSON Schema's default.
 *)
val print :
  ?version:version ->
  ?xprop:bool ->
  src_name:string ->
  root_type:string ->
  out_channel -> Ast.full_module -> unit
