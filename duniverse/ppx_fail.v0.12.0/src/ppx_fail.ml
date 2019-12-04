open Base
open Ppxlib

let expand (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_apply (id, args) when
      not (List.exists args ~f:(fun (lab, _) ->
        match lab with
        | Labelled "here" -> true
        | _ -> false)) ->
    let here = Ppx_here_expander.lift_position ~loc:id.pexp_loc in
    Some { e with pexp_desc = Pexp_apply (id, (Labelled "here", here) :: args) }
  | Pexp_ident _ ->
    (* This case is a bit dubious but it's what was done before *)
    let loc = e.pexp_loc in
    let here = Ppx_here_expander.lift_position ~loc in
    Some (Ast_builder.Default.pexp_apply ~loc e [(Labelled "here", here)])
  | _ -> None
;;

let () =
  Driver.register_transformation "fail"
    ~rules:[ Context_free.Rule.special_function "failwiths" expand ]
;;
