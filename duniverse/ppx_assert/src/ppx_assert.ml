open Ppxlib

let expand_test_pred ~loc:_ ~path:_ typ =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  [%expr fun ?(here= []) ?message predicate t ->
       let pos       = [%e Ppx_here_expander.lift_position_as_string ~loc] in
       let sexpifier = [%e Ppx_sexp_conv_expander.Sexp_of.core_type typ] in
       Ppx_assert_lib.Runtime.test_pred
         ~pos ~sexpifier ~here ?message predicate t
  ]
;;


let expand_test_eq ~loc:_ ~path:_ typ =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  [%expr fun ?(here= []) ?message ?equal t1 t2 ->
       let pos        = [%e Ppx_here_expander.lift_position_as_string ~loc] in
       let sexpifier  = [%e Ppx_sexp_conv_expander.Sexp_of.core_type typ] in
       let comparator =
         [%e Merlin_helpers.hide_expression
               (Ppx_compare_expander.Compare.core_type typ) ]
       in
       Ppx_assert_lib.Runtime.test_eq
         ~pos ~sexpifier ~comparator ~here ?message ?equal t1 t2
  ]
;;

let expand_test_result ~loc:_ ~path:_ typ =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  [%expr fun ?(here= []) ?message ?equal ~expect got ->
       let pos        = [%e Ppx_here_expander.lift_position_as_string ~loc] in
       let sexpifier  = [%e Ppx_sexp_conv_expander.Sexp_of.core_type typ] in
       let comparator =
         [%e Merlin_helpers.hide_expression
               (Ppx_compare_expander.Compare.core_type typ) ]
       in
       Ppx_assert_lib.Runtime.test_result
         ~pos ~sexpifier ~comparator ~here ?message ?equal ~expect ~got
  ]
;;

let extensions =
  let declare name expand =
    [ Extension.declare name Extension.Context.expression Ast_pattern.(ptyp __)
        expand;
      Extension.declare name Extension.Context.core_type Ast_pattern.(ptyp __)
        (fun ~loc ~path:_ ty ->
           let loc = { loc with loc_ghost = true } in
           let open Ast_builder.Default in
           let ident = Located.lident ~loc ("Ppx_assert_lib.Runtime." ^ name) in
           ptyp_constr ~loc ident [ty]);
    ]
  in
  List.concat
    [ declare "test_pred"   expand_test_pred
    ; declare "test_eq"     expand_test_eq
    ; declare "test_result" expand_test_result
    ]
;;

let () =
  Driver.register_transformation "assert" ~extensions
;;
