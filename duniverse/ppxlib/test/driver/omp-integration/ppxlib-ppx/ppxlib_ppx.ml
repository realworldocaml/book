open Ppxlib

let () =
  Driver.register_transformation "plop"
    ~rules:[
      Context_free.Rule.extension
        (Extension.declare_with_path_arg "plop"
           Expression
           Ast_pattern.(pstr nil)
           (fun ~loc ~path:_ ~arg ->
              let open Ast_builder.Default in
              match arg with
              | None -> estring ~loc "-"
              | Some { loc; txt } -> estring ~loc (Longident.name txt)))]
