open Base
open Ppxlib
open Ast_builder.Default

let omit_nil =
  Attribute.declare "sexp_value.sexp.omit_nil"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()

let option =
  Attribute.declare "sexp_value.sexp.option"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()

let sexp_atom ~loc x = [%expr Ppx_sexp_conv_lib.Sexp.Atom [%e x]]
let sexp_list ~loc x = [%expr Ppx_sexp_conv_lib.Sexp.List [%e x]]

let rec list_and_tail_of_ast_list rev_el e =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "::"; _ },
                    Some { pexp_desc = Pexp_tuple [hd; tl]; _ }) ->
    list_and_tail_of_ast_list (hd :: rev_el) tl
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> List.rev rev_el, None
  | _ -> List.rev rev_el, Some e
;;

let sexp_of_constant ~loc const =
  let f typ =
    eapply ~loc (evar ~loc ("Ppx_sexp_conv_lib.Conv.sexp_of_" ^ typ)) [pexp_constant ~loc const]
  in
  match const with
  | Pconst_integer   _ -> f "int"
  | Pconst_char      _ -> f "char"
  | Pconst_string    _ -> f "string"
  | Pconst_float     _ -> f "float"
;;

type omittable_sexp =
  | Present of expression
  | Optional of (Location.t * string) * expression * (expression -> expression)
  (* In [Optional (_, e, k)], [e] is an ast whose values have type ['a option], and [k] is
     a function from ast of type ['a] to ast of type [Sexp.t]. The None case should not be
     displayed, and the [a] in the Some case should be displayed by calling [k] on it. *)
  | Omit_nil of Location.t * expression * (expression -> expression)
  (* In [Omit_nil (_, e, k)], [e] is an ast of type [Sexp.t], and [k] if a function
     ast of type [Sexp.t] and returns an other [Sexp.t].
     When [e] is [List []], it should be not displayed. Otherwise [e] should be
     displayed by calling [k] on it. *)

let wrap_sexp_if_present omittable_sexp ~f =
  match omittable_sexp with
  | Optional (loc, e, k) -> Optional (loc, e, (fun e -> f (k e)))
  | Present e -> Present (f e)
  | Omit_nil (loc, e, k) -> Omit_nil (loc, e, (fun e -> f (k e)))

let sexp_of_constraint ~loc expr ctyp =
  match ctyp with
  | [%type: [%t? ty] sexp_option] ->
    let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ty in
    Optional ((loc, "sexp_option"), expr, fun expr -> eapply ~loc sexp_of [expr])
  | [%type: [%t? ty] option] when Option.is_some (Attribute.get option ctyp) ->
    let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ty in
    Optional ((loc, "[@sexp.optional]"), expr, fun expr -> eapply ~loc sexp_of [expr])
  | _ ->
    let expr =
      let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ctyp in
      eapply ~loc sexp_of [expr]
    in
    match Attribute.get omit_nil ctyp with
    | Some () -> Omit_nil (loc, expr, Fn.id)
    | None -> Present expr
;;

let rec sexp_of_expr expr =
  match omittable_sexp_of_expr expr with
  | Present v -> v
  | Optional ((loc, s), _, _) ->
    Location.raise_errorf ~loc
      "ppx_sexp_value: cannot handle %s in this context" s
  | Omit_nil (loc, _, _) ->
    Location.raise_errorf ~loc
      "ppx_sexp_value: cannot handle [@omit_nil] in this context"

and omittable_sexp_of_expr expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  wrap_sexp_if_present ~f:(fun new_expr ->
    { new_expr with pexp_attributes = expr.pexp_attributes })
    (match expr.pexp_desc with
     | Pexp_ifthenelse (e1, e2, e3) ->
       Present
         { expr with
           pexp_desc =
             Pexp_ifthenelse (e1, sexp_of_expr e2,
                              match e3 with
                              | None -> None
                              | Some e -> Some (sexp_of_expr e))
         }
     | Pexp_constraint (expr, ctyp) ->
       sexp_of_constraint ~loc expr ctyp
     | Pexp_construct ({ txt = Lident "[]"; _ }, None)
     | Pexp_construct ({ txt = Lident "::"; _ },
                       Some { pexp_desc = Pexp_tuple [_; _]; _ }) ->
       let el, tl = list_and_tail_of_ast_list [] expr in
       let el = List.map el ~f:omittable_sexp_of_expr in
       let tl =
         match tl with
         | None -> [%expr [] ]
         | Some e ->
           [%expr
             match [%e sexp_of_expr e] with
             | Ppx_sexp_conv_lib.Sexp.List l -> l
             | Ppx_sexp_conv_lib.Sexp.Atom _ as sexp -> [sexp]
           ]
       in
       Present (sexp_of_omittable_sexp_list loc el ~tl)
     | Pexp_constant const ->
       Present (sexp_of_constant ~loc const)
     | Pexp_extension ({ txt = "here"; _ }, PStr []) ->
       Present (sexp_atom ~loc (Ppx_here_expander.lift_position_as_string ~loc))
     | Pexp_construct ({ txt = Lident "()"; _ }, None) ->
       Present (sexp_list ~loc (elist ~loc []))
     | Pexp_construct ({ txt = Lident constr; _ }, None)
     | Pexp_variant   (               constr     , None) ->
       Present (sexp_atom ~loc (estring ~loc constr))
     | Pexp_construct ({ txt = Lident constr; _ }, Some arg)
     | Pexp_variant   (               constr     , Some arg) ->
       let k hole =
         sexp_list ~loc
           (elist ~loc [ sexp_atom ~loc (estring ~loc constr)
                       ; hole
                       ])
       in
       wrap_sexp_if_present (omittable_sexp_of_expr arg) ~f:k
     | Pexp_tuple el ->
       let el = List.map el ~f:omittable_sexp_of_expr in
       Present (sexp_of_omittable_sexp_list loc el ~tl:(elist ~loc []))
     | Pexp_record (fields, None) ->
       Present (sexp_of_record ~loc fields)
     | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "~~"; _ }; _},
                   [ (Nolabel, { pexp_desc = Pexp_constraint (expr, ctyp); _ }) ]) ->
       let expr_str = Pprintast.string_of_expression expr in
       let k hole =
         sexp_list ~loc
           (elist ~loc [ sexp_atom ~loc (estring ~loc expr_str)
                       ; hole
                       ])
       in
       wrap_sexp_if_present (sexp_of_constraint ~loc expr ctyp) ~f:k
     | _ ->
       Location.raise_errorf ~loc
         "ppx_sexp_value: don't know how to handle this construct"
    )

and sexp_of_omittable_sexp_list loc el ~tl =
  let l =
    List.fold_left (List.rev el) ~init:tl ~f:(fun acc e ->
      match e with
      | Present e -> [%expr [%e e] :: [%e acc] ]
      | Optional (_, v_opt, k) ->
        (* We match simultaneously on the head and tail in the generated code to avoid
           changing their respective typing environments. *)
        [%expr
          match [%e v_opt], [%e acc ] with
          | None, tl -> tl
          | Some v, tl -> [%e k [%expr v]] :: tl
        ]
      | Omit_nil (_, e, k) ->
        [%expr
          match [%e e], [%e acc] with
          | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
          | v, tl -> [%e k [%expr v]] :: tl
        ]
    )
  in
  sexp_list ~loc l

and sexp_of_record ~loc fields =
  sexp_of_omittable_sexp_list loc ~tl:(elist ~loc [])
    (List.map fields ~f:(fun (id, e) ->
       let e =
         match e.pexp_desc with
         | Pexp_constraint (e', c)
           when Location.compare_pos id.loc.loc_start e.pexp_loc.loc_start = 0
             && Location.compare e.pexp_loc e'.pexp_loc = 0 ->
           (* { foo : int }  *)
           { e with
             pexp_desc = Pexp_constraint ({ e' with pexp_loc = id.loc}, c) }
         | _ -> e
       in
       let loc = { id.loc with loc_end = e.pexp_loc.loc_end; loc_ghost = true } in
       let name = String.concat ~sep:"." (Longident.flatten_exn id.txt) in
       let k hole =
         sexp_list ~loc
           (elist ~loc
              [ sexp_atom ~loc (estring ~loc:{ id.loc with loc_ghost = true } name)
              ; hole ])
       in
       wrap_sexp_if_present (omittable_sexp_of_expr e) ~f:k))
;;

let () =
  Driver.register_transformation "sexp_value"
    ~extensions:[
      Extension.declare "sexp"
        Extension.Context.expression
        Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
        (fun ~loc:_ ~path:_ e -> sexp_of_expr e)
    ]
;;
