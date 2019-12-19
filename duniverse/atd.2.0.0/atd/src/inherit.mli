(** Expansion of [inherit] statements *)

val expand_module_body :
  ?inherit_fields : bool ->
  ?inherit_variants : bool ->
  Ast.module_body -> Ast.module_body
  (**
     Expand [inherit] statements found in sum types and product types.

     @param inherit_fields specify whether record fields should be expanded.
     Default is true.

     @param inherit_variants specify whether sum types
     should be expanded. Default is true.
  *)
