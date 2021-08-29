(* Generated code should depend on the environment in scope as little as possible.
   E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=].  It
   is especially important to not use polymorphic comparisons, since we are moving more
   and more to code that doesn't have them in scope. *)


open Base
open Ppxlib
open Ast_builder.Default

let raise_unsupported loc =
  Location.raise_errorf ~loc
    "Unsupported use of variants (you can only use it on variant types)."

module Create = struct
  let lambda loc xs body =
    List.fold_right xs ~init:body ~f:(fun (label, p) e -> pexp_fun ~loc label None p e)
  ;;

  let lambda_sig loc arg_tys body_ty =
    List.fold_right arg_tys ~init:body_ty ~f:(fun (label, arg_ty) acc ->
      ptyp_arrow ~loc label arg_ty acc)
  ;;
end

module Variant_constructor = struct
  type t = {
    name    : string;
    loc     : Location.t;
    kind    : [ `Normal of core_type list
              | `Normal_inline_record of label_declaration list
              | `Polymorphic of core_type option ]
  }

  let args t =
    match t.kind with
    | `Normal pcd_args ->
      List.mapi pcd_args ~f:(fun i _ -> Nolabel, "v" ^ Int.to_string i)
    | `Normal_inline_record fields ->
      List.mapi fields ~f:(fun i f -> Labelled f.pld_name.txt, "v" ^ Int.to_string i)
    | `Polymorphic None -> []
    | `Polymorphic (Some _) -> [Nolabel, "v0"]

  let pattern_without_binding { name; loc; kind } =
    match kind with
    | `Normal [] ->
      ppat_construct ~loc (Located.lident ~loc name) None
    | `Normal (_ :: _) | `Normal_inline_record _ ->
      ppat_construct ~loc (Located.lident ~loc name) (Some (ppat_any ~loc))
    | `Polymorphic None ->
      ppat_variant ~loc name None
    | `Polymorphic (Some _) ->
      ppat_variant ~loc name (Some (ppat_any ~loc))

  let to_fun_type t ~rhs:body_ty =
    let arg_types =
      match t.kind with
      | `Polymorphic None -> []
      | `Polymorphic (Some v) -> [(Nolabel, v)]
      | `Normal args -> List.map args ~f:(fun typ -> Nolabel, typ)
      | `Normal_inline_record fields ->
        List.map fields ~f:(fun cd -> Labelled cd.pld_name.txt, cd.pld_type)
    in
    Create.lambda_sig t.loc arg_types body_ty
end

let variant_name_to_string v =
  let s = String.lowercase v in
  if Caml.Hashtbl.mem Lexer.keyword_table s
  then s ^ "_"
  else s

module Inspect = struct
  let row_field loc rf : Variant_constructor.t =
    match rf.prf_desc with
    | Rtag ({ txt = name; _ }, true, _) | Rtag ({ txt = name; _ }, _, []) ->
      { name
      ; loc
      ; kind = `Polymorphic None
      }
    | Rtag ({ txt = name; _}, false, tp :: _) ->
      { name
      ; loc
      ; kind = `Polymorphic (Some tp)
      }
    | Rinherit _ ->
      Location.raise_errorf ~loc
        "ppx_variants_conv: polymorphic variant inclusion is not supported"

  let constructor cd : Variant_constructor.t =
    if Option.is_some cd.pcd_res then
      Location.raise_errorf ~loc:cd.pcd_loc "GADTs are not supported by variantslib";
    let kind =
      match cd.pcd_args with
      | Pcstr_tuple pcd_args -> `Normal pcd_args
      | Pcstr_record fields  -> `Normal_inline_record fields
    in
    { name = cd.pcd_name.txt
    ; loc = cd.pcd_name.loc
    ; kind
    }

  let type_decl td =
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_variant cds ->
      let cds = List.map cds ~f:constructor in
      let names_as_string = Hashtbl.create (module String) in
      List.iter cds ~f:(fun { name; loc; _ } ->
        let s = variant_name_to_string name in
        match Hashtbl.find names_as_string s with
        | None -> Hashtbl.add_exn names_as_string ~key:s ~data:name
        | Some name' ->
          Location.raise_errorf ~loc
            "ppx_variants_conv: constructors %S and %S both get mapped to value %S"
            name name' s
      );
      cds
    | Ptype_record _ | Ptype_open -> raise_unsupported loc
    | Ptype_abstract ->
      match td.ptype_manifest with
      | Some { ptyp_desc = Ptyp_variant (row_fields, Closed, None); _ } ->
        List.map row_fields ~f:(row_field loc)
      | Some { ptyp_desc = Ptyp_variant _; ptyp_loc = loc; _ } ->
        Location.raise_errorf ~loc
          "ppx_variants_conv: polymorphic variants with a row variable are not supported"
      | _ -> raise_unsupported loc
end

let variants_module = function
  | "t" -> "Variants"
  | type_name -> "Variants_of_" ^ type_name
;;

module Gen_sig = struct
  let apply_type loc ~ty_name ~tps =
    ptyp_constr ~loc (Located.lident ~loc ty_name) tps

  let label_arg _loc name ty =
    (Asttypes.Labelled (variant_name_to_string name), ty)
  ;;

  let val_ ~loc name type_ =
    psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc name) ~type_ ~prim:[])
  ;;

  let variant_arg f ~variant_type (v : Variant_constructor.t) =
    let loc = v.loc in
    let variant =
      [%type: [%t Variant_constructor.to_fun_type v ~rhs:variant_type] Variantslib.Variant.t]
    in
    label_arg loc v.Variant_constructor.name (f ~variant)
  ;;

  let v_fold_fun ~variant_type loc variants =
    let f = variant_arg ~variant_type (fun ~variant -> [%type: 'acc__ -> [%t variant] -> 'acc__ ]) in
    let types = List.map variants ~f in
    let init_ty = label_arg loc "init" [%type:  'acc__ ] in
    let t = Create.lambda_sig loc (init_ty :: types) [%type: 'acc__ ] in
    val_ ~loc "fold" t
  ;;

  let v_iter_fun ~variant_type loc variants =
    let f = variant_arg ~variant_type (fun ~variant -> [%type:  [%t variant] -> unit]) in
    let types = List.map variants ~f in
    let t = Create.lambda_sig loc types [%type:  unit ] in
    val_ ~loc "iter" t
  ;;

  let v_map_fun ~variant_type loc variants =
    let module V = Variant_constructor in
    let result_type = [%type:  'result__ ] in
    let f v =
      let variant =
        let constructor_type = V.to_fun_type v ~rhs:variant_type in
        Create.lambda_sig loc
          [ Nolabel, [%type: [%t constructor_type] Variantslib.Variant.t ] ]
          (V.to_fun_type v ~rhs:result_type)
      in
      label_arg loc v.V.name variant
    in
    let types = List.map variants ~f in
    let t = Create.lambda_sig loc ((Nolabel, variant_type) :: types) result_type in
    val_ ~loc "map" t
  ;;

  let v_make_matcher_fun ~variant_type loc variants =
    let result_type = [%type:  'result__ ] in
    let acc i = ptyp_var ~loc ("acc__" ^ Int.to_string i) in
    let f i v =
      let variant =
        [%type: [%t Variant_constructor.to_fun_type v ~rhs:variant_type] Variantslib.Variant.t]
      in
      let fun_type =
        match Variant_constructor.args v with
        | [] -> [%type: unit -> [%t result_type]]
        | ( _::_ ) -> Variant_constructor.to_fun_type v ~rhs:result_type
      in
      label_arg loc v.name [%type: [%t variant] -> [%t acc i] -> [%t fun_type] * [%t acc (i+1)]]
    in
    let types = List.mapi variants ~f in
    let t = Create.lambda_sig loc (types @ [Nolabel, acc 0])
              [%type: ([%t variant_type] -> [%t result_type]) * [%t (acc (List.length variants))]] in
    val_ ~loc "make_matcher" t
  ;;

  let v_descriptions ~variant_type:_ loc _ =
    val_ ~loc "descriptions" [%type: (string * int) list]

  let v_to_rank_fun ~variant_type loc _ =
    val_ ~loc "to_rank" [%type: [%t variant_type] -> int]
  ;;

  let v_to_name_fun ~variant_type loc _ =
    val_ ~loc "to_name" [%type: [%t variant_type] -> string]
  ;;

  let variant ~variant_type ~ty_name loc variants =
    let constructors, variant_defs =
      List.unzip (List.map variants ~f:(fun v ->
        let module V = Variant_constructor in
        let constructor_type = V.to_fun_type v ~rhs:variant_type in
        let name = variant_name_to_string v.V.name in
        ( val_ ~loc name constructor_type
        , val_ ~loc name [%type: [%t constructor_type] Variantslib.Variant.t]
        )))
    in
    constructors @
    [ psig_module ~loc
        (module_declaration
           ~loc
           ~name:(Located.mk ~loc (Some (variants_module ty_name)))
           ~type_:(pmty_signature ~loc
                     (variant_defs @ [ v_fold_fun ~variant_type loc variants
                                     ; v_iter_fun ~variant_type loc variants
                                     ; v_map_fun ~variant_type loc variants
                                     ; v_make_matcher_fun ~variant_type loc variants
                                     ; v_to_rank_fun ~variant_type loc variants
                                     ; v_to_name_fun ~variant_type loc variants
                                     ; v_descriptions ~variant_type loc variants
                                     ])))
    ]
  ;;

  let variants_of_td td =
    let ty_name = td.ptype_name.txt in
    let loc = td.ptype_loc in
    let variant_type = apply_type loc ~ty_name ~tps:(List.map td.ptype_params ~f:fst) in
    variant ~variant_type ~ty_name loc (Inspect.type_decl td)

  let generate ~loc ~path:_ (rec_flag, tds) =
    (match rec_flag with
     | Nonrecursive ->
       Location.raise_errorf ~loc
         "nonrec is not compatible with the `ppx_variants_conv' preprocessor"
     | _ -> ());
    match tds with
    | [td] -> variants_of_td td
    | _ -> Location.raise_errorf ~loc "ppx_variants_conv: not supported"
end

module Gen_str = struct

  let constructors_and_variants loc variants =
    let module V = Variant_constructor in
    List.unzip
      (List.mapi variants ~f:(fun rank v ->
         let uncapitalized = variant_name_to_string v.V.name in
         let constructor =
           let constructed_value =
             match v.V.kind with
             | `Normal _      ->
               let arg = pexp_tuple_opt ~loc (List.map (V.args v) ~f:(fun (_,v) -> evar ~loc v)) in
               pexp_construct ~loc (Located.lident ~loc v.V.name) arg
             | `Polymorphic _ ->
               let arg = pexp_tuple_opt ~loc (List.map (V.args v) ~f:(fun (_,v) -> evar ~loc v)) in
               pexp_variant   ~loc                  v.V.name  arg
             | `Normal_inline_record fields ->
               let arg =
                 pexp_record ~loc
                   (List.map2_exn fields (V.args v) ~f:(fun f (_,name) ->
                      Located.lident ~loc f.pld_name.txt, evar ~loc name))
                   None
               in
               pexp_construct ~loc (Located.lident ~loc v.V.name) (Some arg)
           in
           pstr_value ~loc Nonrecursive
             [ value_binding ~loc ~pat:(pvar ~loc uncapitalized)
                 ~expr:(List.fold_right (V.args v)
                          ~init:constructed_value
                          ~f:(fun (label,v) e ->
                            pexp_fun ~loc label None (pvar ~loc v) e)
                       )
             ]
         in
         let variant =
           [%stri
             let [%p pvar ~loc uncapitalized] =
               { Variantslib.Variant.
                 name        = [%e estring ~loc v.V.name     ]
               ; rank        = [%e eint    ~loc rank         ]
               ; constructor = [%e evar    ~loc uncapitalized]
               }
           ]
         in
         constructor, variant
       ))
  ;;

  let label_arg ?label loc name =
    let l =
      match label with
      | None    -> name
      | Some n  -> n
    in
    (Asttypes.Labelled l, pvar ~loc name)
  ;;

  let label_arg_fun loc name =
    label_arg ~label:name loc (name ^ "_fun__")
  ;;

  let v_fold_fun loc variants =
    let module V = Variant_constructor in
    let variant_fold acc_expr variant =
      let variant_name = variant_name_to_string variant.V.name in
      [%expr  [%e evar ~loc (variant_name ^ "_fun__")] [%e acc_expr]
                [%e evar ~loc variant_name]
      ]
    in
    let body =
      List.fold_left variants ~init:[%expr  init__ ] ~f:variant_fold
    in
    let patterns =
      List.map variants ~f:(fun variant ->
        label_arg_fun loc (variant_name_to_string variant.V.name))
    in
    let init = label_arg ~label:"init" loc "init__" in
    let lambda = Create.lambda loc (init :: patterns) body in
    [%stri let fold = [%e lambda] ]
  ;;

  let v_descriptions loc variants =
    let module V = Variant_constructor in
    let f v =
      [%expr
        ( [%e estring ~loc v.V.name]
        , [%e eint ~loc (List.length (V.args v))]
        )
      ]
    in
    let variant_names = List.map ~f variants in
    [%stri let descriptions = [%e elist ~loc variant_names] ]
  ;;

  let v_map_fun loc variants =
    let module V = Variant_constructor in
    let variant_match_case variant =
      let pattern =
        match variant.V.kind with
        | `Polymorphic _ ->
          let arg = ppat_tuple_opt ~loc (List.map (V.args variant) ~f:(fun (_,v) -> pvar ~loc v)) in
          ppat_variant   ~loc                  variant.V.name  arg
        | `Normal _      ->
          let arg = ppat_tuple_opt ~loc (List.map (V.args variant) ~f:(fun (_,v) -> pvar ~loc v)) in
          ppat_construct ~loc (Located.lident ~loc variant.V.name) arg
        | `Normal_inline_record fields ->
          let arg =
            ppat_record ~loc
              (List.map2_exn fields (V.args variant)
                 ~f:(fun f (_,v) -> Located.lident ~loc f.pld_name.txt, pvar ~loc v))
              Closed
          in
          ppat_construct ~loc (Located.lident ~loc variant.V.name) (Some arg)
      in
      let uncapitalized = variant_name_to_string variant.V.name in
      let value =
        List.fold_left (V.args variant)
          ~init:(eapply ~loc (evar ~loc (uncapitalized ^ "_fun__")) [evar ~loc uncapitalized])
          ~f:(fun acc_expr (label, var) -> pexp_apply ~loc acc_expr [label, evar ~loc var])
      in
      case ~guard:None ~lhs:pattern ~rhs:value
    in
    let body = pexp_match ~loc [%expr t__] (List.map variants ~f:variant_match_case) in
    let patterns =
      List.map variants ~f:(fun variant ->
        label_arg_fun loc (variant_name_to_string variant.V.name))
    in
    let lambda = Create.lambda loc ((Nolabel, [%pat? t__]) :: patterns) body in
    [%stri let map = [%e lambda] ]
  ;;

  let v_iter_fun loc variants =
    let module V = Variant_constructor in
    let names = List.map variants ~f:(fun v -> variant_name_to_string v.V.name) in
    let variant_iter variant =
      let variant_name = variant_name_to_string variant.V.name in
      [%expr ([%e evar ~loc (variant_name ^ "_fun__")] [%e evar ~loc variant_name] : unit) ]
    in
    let body = esequence ~loc (List.map variants ~f:variant_iter) in
    let patterns = List.map names ~f:(label_arg_fun loc) in
    let lambda = Create.lambda loc patterns body in
    [%stri let iter = [%e lambda] ]
  ;;

  let v_make_matcher_fun loc variants =
    let module V = Variant_constructor in
    let result =
      let map =
        List.fold_left variants
          ~init:[%expr map]
          ~f:(fun acc variant ->
          let variant_name = variant_name_to_string variant.V.name in
          pexp_apply ~loc acc
            [Labelled variant_name,
             match V.args variant with
             | [] -> [%expr fun _ -> [%e evar ~loc (variant_name ^ "_gen__")] ()]
             | (_::_) ->
               [%expr fun _ ->
                 [%e evar ~loc (variant_name ^ "_gen__")]]])
      in
      [%expr [%e map], compile_acc__]
    in
    let body =
      List.fold_right variants
        ~init:result
        ~f:(fun variant acc ->
          let variant_name = variant_name_to_string variant.V.name in
          pexp_let ~loc Nonrecursive [
            value_binding ~loc
              ~pat:(ppat_tuple ~loc [
                [%pat? [%p pvar ~loc (variant_name ^ "_gen__")]];
                [%pat? compile_acc__];
              ])
              ~expr:[%expr
                [%e evar ~loc (variant_name ^ "_fun__")]
                  [%e evar ~loc variant_name]
                  compile_acc__]
          ] acc)
    in
    let patterns = List.map variants ~f:(fun v -> label_arg_fun loc (variant_name_to_string v.V.name)) in
    let lambda = Create.lambda loc (patterns @ [ Nolabel, [%pat? compile_acc__ ]]) body in
    [%stri let make_matcher = [%e lambda] ]
  ;;

  let case_analysis_ignoring_values variants ~f =
    let pattern_and_rhs =
      List.mapi variants ~f:(fun rank v ->
        Variant_constructor.pattern_without_binding v, f ~rank ~name:v.name)
    in
    List.map pattern_and_rhs ~f:(fun (pattern, rhs) ->
      case ~guard:None ~lhs:pattern ~rhs)
  ;;

  let v_to_rank loc ty =
    let cases =
      case_analysis_ignoring_values ty
        ~f:(fun ~rank ~name:_ -> eint ~loc rank)
    in
    [%stri let to_rank = [%e pexp_function ~loc cases]]
  ;;

  let v_to_name loc ty =
    let cases =
      case_analysis_ignoring_values ty
        ~f:(fun ~rank:_ ~name -> estring ~loc name)
    in
    [%stri let to_name = [%e pexp_function ~loc cases]]
  ;;

  let variant ~variant_name loc ty =
    let constructors, variants = constructors_and_variants loc ty in
    constructors @
    [ pstr_module ~loc
        (module_binding
           ~loc
           ~name:(Located.mk ~loc (Some (variants_module variant_name)))
           ~expr:(pmod_structure ~loc
                    (variants @ [ v_fold_fun loc ty
                                ; v_iter_fun loc ty
                                ; v_map_fun loc ty
                                ; v_make_matcher_fun loc ty
                                ; v_to_rank loc ty
                                ; v_to_name loc ty
                                ; v_descriptions loc ty
                                ])))
    ]
  ;;

  let variants_of_td td =
    let variant_name = td.ptype_name.txt in
    let loc = td.ptype_loc in
    variant ~variant_name loc (Inspect.type_decl td)

  let generate ~loc ~path:_ (rec_flag, tds) =
    (match rec_flag with
     | Nonrecursive ->
       Location.raise_errorf ~loc
         "nonrec is not compatible with the `ppx_variants_conv' preprocessor"
     | _ -> ());
    match tds with
    | [td] -> variants_of_td td
    | _ -> Location.raise_errorf ~loc "ppx_variants_conv: not supported"
end

let variants =
  Deriving.add "variants"
    ~str_type_decl:(Deriving.Generator.make_noarg Gen_str.generate)
    ~sig_type_decl:(Deriving.Generator.make_noarg Gen_sig.generate)
;;
