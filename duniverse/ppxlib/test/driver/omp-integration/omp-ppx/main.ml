open Migrate_parsetree
open Ast_403

let mapper =
  let super = Ast_mapper.default_mapper in
  let expr self (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_extension ({ txt = "omp_test"; _ }, _) ->
      { e with pexp_desc = Pexp_constant (Pconst_integer ("42", None)) }
    | _ ->
      super.expr self e
  in
  { super with expr }

let () =
  Driver.register ~name:"omp_test"
    (module OCaml_403)
    (fun _ _ -> mapper)
