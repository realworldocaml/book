open Ppxlib

let export_string ~ctxt e =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let open Ast_builder.Default in
  estring ~loc e

let export_string_extension =
  Extension.V3.declare "export_string" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    export_string

let rule = Ppxlib.Context_free.Rule.extension export_string_extension
let () = Driver.register_transformation ~rules:[ rule ] "export_string"
let () = Driver.standalone ()
