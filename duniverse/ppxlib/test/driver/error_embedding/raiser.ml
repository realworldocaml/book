open Ppxlib

let rule =
  let expand ~loc ~path:_ =
    Location.raise_errorf ~loc "Raising inside the rewriter"
  in
  Extension.declare "raise" Extension.Context.expression
    Ast_pattern.(pstr nil)
    expand
  |> Context_free.Rule.extension

let () = Driver.register_transformation ~rules:[ rule ] "test"
let () = Driver.standalone ()
