open Ppxlib

let generate_impl_extension_node ~ctxt (_rec_flag, _type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let extension_node =
    Location.error_extensionf ~loc "An error message in an extension node"
  in
  [ Ast_builder.Default.pstr_extension ~loc extension_node [] ]

let generate_impl_located_error ~ctxt (_rec_flag, _type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  Location.raise_errorf ~loc "A raised located error"

let generate_impl_raised_exception ~ctxt:_ (_rec_flag, _type_declarations) =
  failwith "A raised exception"

let impl_generator_extension_node =
  Deriving.Generator.V2.make_noarg generate_impl_extension_node

let impl_generator_located_error =
  Deriving.Generator.V2.make_noarg generate_impl_located_error

let impl_generator_raised_exception =
  Deriving.Generator.V2.make_noarg generate_impl_raised_exception

let my_deriver_extension_node =
  Deriving.add "deriver_extension_node"
    ~str_type_decl:impl_generator_extension_node

let my_deriver_located_error =
  Deriving.add "deriver_located_error"
    ~str_type_decl:impl_generator_located_error

let my_deriver_raised_exception =
  Deriving.add "deriver_raised_exception"
    ~str_type_decl:impl_generator_raised_exception

let () = Driver.standalone ()
