open Ppxlib

let ext extension_name =
  Extension.declare_with_path_arg
    (Ppx_let_expander.Extension_name.to_string extension_name)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg expr ->
       Ppx_let_expander.expand extension_name ~modul:arg expr)
;;

let () =
  Driver.register_transformation
    "let"
    ~extensions:
      [ ext Sub
      ; ext Sub_open
      ; ext Bind
      ; ext Bind_open
      ; ext Map
      ; ext Map_open
      ; ext Mapn
      ; ext Mapn_open
      ; ext Bindn
      ; ext Bindn_open
      ]
;;
