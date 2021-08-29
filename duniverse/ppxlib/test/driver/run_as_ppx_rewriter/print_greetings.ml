open Ppxlib

let hi_rule =
  let expand ~loc ~path:_ = [%expr print_endline "hi"] in
  Extension.declare "print_hi" Extension.Context.expression
    Ast_pattern.(pstr nil)
    expand
  |> Context_free.Rule.extension

let bye_rule =
  let expand ~loc ~path:_ = [%expr print_endline "bye"] in
  Extension.declare "print_bye" Extension.Context.expression
    Ast_pattern.(pstr nil)
    expand
    |> Context_free.Rule.extension

(* the two rules need to be registered separately in order to test the `-apply` flag in run.t *)
let () = Driver.register_transformation ~rules:[ hi_rule ] "print_hi"

let () = Driver.register_transformation ~rules:[ bye_rule ] "print_bye"

let () = Ppxlib.Driver.run_as_ppx_rewriter ()
