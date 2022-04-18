open Ppxlib

let expand ~ctxt env_var =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match Sys.getenv env_var with
  | value -> Ast_builder.Default.estring ~loc value
  | exception Not_found ->
      let ext =
        Location.error_extensionf ~loc "The environement variable %s is unbound"
          env_var
      in
      Ast_builder.Default.pexp_extension ~loc ext

let my_extension =
  Extension.V3.declare "get_env" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "get_env"
