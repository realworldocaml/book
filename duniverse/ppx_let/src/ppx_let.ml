open Ppxlib

let ext t extension_kind =
  Extension.declare_with_path_arg
    (Ppx_let_expander.ext_full_name t extension_kind)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg expr ->
       Ppx_let_expander.expand t extension_kind ~modul:arg expr)
;;

open Ppx_let_expander

let () =
  Driver.register_transformation
    "let"
    ~extensions:
      [ ext bind Extension_kind.default
      ; ext bind Extension_kind.default_open
      ; ext bind Extension_kind.n
      ; ext bind Extension_kind.n_open
      ; ext map Extension_kind.default
      ; ext map Extension_kind.default_open
      ; ext map Extension_kind.n
      ; ext map Extension_kind.n_open
      ; ext sub Extension_kind.default
      ; ext sub Extension_kind.default_open
      ; ext arr Extension_kind.default
      ; ext arr Extension_kind.default_open
      ]
;;
