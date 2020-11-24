open Ppxlib

let here =
  Extension.declare "here" Extension.Context.expression
    Ast_pattern.(pstr nil)
    (fun ~loc ~path:_ -> Ppx_here_expander.lift_position ~loc)
;;

let () =
  Driver.register_transformation "here"
    ~extensions:[ here ]
;;
