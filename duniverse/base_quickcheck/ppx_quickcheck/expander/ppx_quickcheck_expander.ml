open! Import

let custom_extension ~loc tag payload =
  match String.equal tag.txt "custom" with
  | false -> unsupported ~loc "uknown extension: %s" tag.txt
  | true ->
    (match payload with
     | PStr [ { pstr_desc = Pstr_eval (expr, attributes); _ } ] ->
       assert_no_attributes attributes;
       expr
     | _ -> invalid ~loc "[%%custom] extension expects a single expression as its payload")
;;

let generator_attribute =
  Attribute.declare
    "quickcheck.generator"
    Attribute.Context.core_type
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)
;;

let rec generator_of_core_type core_type ~gen_env ~obs_env =
  let loc = { core_type.ptyp_loc with loc_ghost = true } in
  match Attribute.get generator_attribute core_type with
  | Some expr -> expr
  | None ->
    (match core_type.ptyp_desc with
     | Ptyp_constr (constr, args) ->
       type_constr_conv
         ~loc
         ~f:generator_name
         constr
         (List.map args ~f:(generator_of_core_type ~gen_env ~obs_env))
     | Ptyp_var tyvar -> Environment.lookup gen_env ~loc ~tyvar
     | Ptyp_arrow (arg_label, input_type, output_type) ->
       Ppx_generator_expander.arrow
         ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
         ~observer_of_core_type:(observer_of_core_type ~gen_env ~obs_env)
         ~loc
         ~arg_label
         ~input_type
         ~output_type
     | Ptyp_tuple fields ->
       Ppx_generator_expander.compound
         ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
         ~loc
         ~fields
         (module Field_syntax.Tuple)
     | Ptyp_variant (clauses, Closed, None) ->
       Ppx_generator_expander.variant
         ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
         ~loc
         ~variant_type:core_type
         ~clauses
         ~rec_names:(Set.empty (module String))
         (module Clause_syntax.Polymorphic_variant)
     | Ptyp_variant (_, Open, _) -> unsupported ~loc "polymorphic variant type with [>]"
     | Ptyp_variant (_, _, Some _) -> unsupported ~loc "polymorphic variant type with [<]"
     | Ptyp_extension (tag, payload) -> custom_extension ~loc tag payload
     | Ptyp_any
     | Ptyp_object _
     | Ptyp_class _
     | Ptyp_alias _
     | Ptyp_poly _
     | Ptyp_package _ -> unsupported ~loc "%s" (short_string_of_core_type core_type))

and observer_of_core_type core_type ~obs_env ~gen_env =
  let loc = { core_type.ptyp_loc with loc_ghost = true } in
  match core_type.ptyp_desc with
  | Ptyp_constr (constr, args) ->
    type_constr_conv
      ~loc
      ~f:observer_name
      constr
      (List.map args ~f:(observer_of_core_type ~obs_env ~gen_env))
  | Ptyp_var tyvar -> Environment.lookup obs_env ~loc ~tyvar
  | Ptyp_arrow (arg_label, input_type, output_type) ->
    Ppx_observer_expander.arrow
      ~observer_of_core_type:(observer_of_core_type ~obs_env ~gen_env)
      ~generator_of_core_type:(generator_of_core_type ~obs_env ~gen_env)
      ~loc
      ~arg_label
      ~input_type
      ~output_type
  | Ptyp_tuple fields ->
    Ppx_observer_expander.compound
      ~observer_of_core_type:(observer_of_core_type ~obs_env ~gen_env)
      ~loc
      ~fields
      (module Field_syntax.Tuple)
  | Ptyp_variant (clauses, Closed, None) ->
    Ppx_observer_expander.variant
      ~observer_of_core_type:(observer_of_core_type ~obs_env ~gen_env)
      ~loc
      ~clauses
      (module Clause_syntax.Polymorphic_variant)
  | Ptyp_variant (_, Open, _) -> unsupported ~loc "polymorphic variant type with [>]"
  | Ptyp_variant (_, _, Some _) -> unsupported ~loc "polymorphic variant type with [<]"
  | Ptyp_extension (tag, payload) -> custom_extension ~loc tag payload
  | Ptyp_any -> Ppx_observer_expander.any ~loc
  | Ptyp_object _ | Ptyp_class _ | Ptyp_alias _ | Ptyp_poly _ | Ptyp_package _ ->
    unsupported ~loc "%s" (short_string_of_core_type core_type)
;;

let rec shrinker_of_core_type core_type ~env =
  let loc = { core_type.ptyp_loc with loc_ghost = true } in
  match core_type.ptyp_desc with
  | Ptyp_constr (constr, args) ->
    type_constr_conv
      ~loc
      ~f:shrinker_name
      constr
      (List.map args ~f:(shrinker_of_core_type ~env))
  | Ptyp_var tyvar -> Environment.lookup env ~loc ~tyvar
  | Ptyp_arrow _ -> Ppx_shrinker_expander.arrow ~loc
  | Ptyp_tuple fields ->
    Ppx_shrinker_expander.compound
      ~shrinker_of_core_type:(shrinker_of_core_type ~env)
      ~loc
      ~fields
      (module Field_syntax.Tuple)
  | Ptyp_variant (clauses, Closed, None) ->
    Ppx_shrinker_expander.variant
      ~shrinker_of_core_type:(shrinker_of_core_type ~env)
      ~loc
      ~variant_type:core_type
      ~clauses
      (module Clause_syntax.Polymorphic_variant)
  | Ptyp_variant (_, Open, _) -> unsupported ~loc "polymorphic variant type with [>]"
  | Ptyp_variant (_, _, Some _) -> unsupported ~loc "polymorphic variant type with [<]"
  | Ptyp_extension (tag, payload) -> custom_extension ~loc tag payload
  | Ptyp_any -> Ppx_shrinker_expander.any ~loc
  | Ptyp_object _ | Ptyp_class _ | Ptyp_alias _ | Ptyp_poly _ | Ptyp_package _ ->
    unsupported ~loc "%s" (short_string_of_core_type core_type)
;;

type impl =
  { loc : location
  ; pat : pattern
  ; var : expression
  ; exp : expression
  }

let generator_impl type_decl ~rec_names =
  let loc = type_decl.ptype_loc in
  let pat = pgenerator type_decl.ptype_name in
  let var = egenerator type_decl.ptype_name in
  let exp =
    let pat_list, `Covariant gen_env, `Contravariant obs_env =
      Environment.create_with_variance
        ~loc
        ~covariant:"generator"
        ~contravariant:"observer"
        type_decl.ptype_params
    in
    let body =
      match type_decl.ptype_kind with
      | Ptype_open -> unsupported ~loc "open type"
      | Ptype_variant clauses ->
        Ppx_generator_expander.variant
          ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
          ~loc
          ~variant_type:[%type: _]
          ~clauses
          ~rec_names
          (module Clause_syntax.Variant)
      | Ptype_record fields ->
        Ppx_generator_expander.compound
          ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
          ~loc
          ~fields
          (module Field_syntax.Record)
      | Ptype_abstract ->
        (match type_decl.ptype_manifest with
         | Some core_type -> generator_of_core_type core_type ~gen_env ~obs_env
         | None -> unsupported ~loc "abstract type")
    in
    List.fold_right pat_list ~init:body ~f:(fun pat body ->
      [%expr fun [%p pat] -> [%e body]])
  in
  { loc; pat; var; exp }
;;

let observer_impl type_decl ~rec_names:_ =
  let loc = type_decl.ptype_loc in
  let pat = pobserver type_decl.ptype_name in
  let var = eobserver type_decl.ptype_name in
  let exp =
    let pat_list, `Covariant obs_env, `Contravariant gen_env =
      Environment.create_with_variance
        ~loc
        ~covariant:"observer"
        ~contravariant:"generator"
        type_decl.ptype_params
    in
    let body =
      match type_decl.ptype_kind with
      | Ptype_open -> unsupported ~loc "open type"
      | Ptype_variant clauses ->
        Ppx_observer_expander.variant
          ~observer_of_core_type:(observer_of_core_type ~obs_env ~gen_env)
          ~loc
          ~clauses
          (module Clause_syntax.Variant)
      | Ptype_record fields ->
        Ppx_observer_expander.compound
          ~observer_of_core_type:(observer_of_core_type ~obs_env ~gen_env)
          ~loc
          ~fields
          (module Field_syntax.Record)
      | Ptype_abstract ->
        (match type_decl.ptype_manifest with
         | Some core_type -> observer_of_core_type core_type ~obs_env ~gen_env
         | None -> unsupported ~loc "abstract type")
    in
    List.fold_right pat_list ~init:body ~f:(fun pat body ->
      [%expr fun [%p pat] -> [%e body]])
  in
  { loc; pat; var; exp }
;;

let shrinker_impl type_decl ~rec_names:_ =
  let loc = type_decl.ptype_loc in
  let pat = pshrinker type_decl.ptype_name in
  let var = eshrinker type_decl.ptype_name in
  let exp =
    let pat_list, env =
      Environment.create ~loc ~prefix:"shrinker" type_decl.ptype_params
    in
    let body =
      match type_decl.ptype_kind with
      | Ptype_open -> unsupported ~loc "open type"
      | Ptype_variant clauses ->
        Ppx_shrinker_expander.variant
          ~shrinker_of_core_type:(shrinker_of_core_type ~env)
          ~loc
          ~variant_type:[%type: _]
          ~clauses
          (module Clause_syntax.Variant)
      | Ptype_record fields ->
        Ppx_shrinker_expander.compound
          ~shrinker_of_core_type:(shrinker_of_core_type ~env)
          ~loc
          ~fields
          (module Field_syntax.Record)
      | Ptype_abstract ->
        (match type_decl.ptype_manifest with
         | Some core_type -> shrinker_of_core_type core_type ~env
         | None -> unsupported ~loc "abstract type")
    in
    List.fold_right pat_list ~init:body ~f:(fun pat body ->
      [%expr fun [%p pat] -> [%e body]])
  in
  { loc; pat; var; exp }
;;

let maybe_mutually_recursive decls ~loc ~rec_flag ~of_lazy ~impl =
  let decls = List.map decls ~f:name_type_params_in_td in
  let rec_names =
    match rec_flag with
    | Nonrecursive -> Set.empty (module String)
    | Recursive ->
      Set.of_list (module String) (List.map decls ~f:(fun decl -> decl.ptype_name.txt))
  in
  let impls = List.map decls ~f:(fun decl -> impl decl ~rec_names) in
  match rec_flag with
  | Nonrecursive ->
    pstr_value_list
      ~loc
      Nonrecursive
      (List.map impls ~f:(fun impl ->
         value_binding ~loc:impl.loc ~pat:impl.pat ~expr:impl.exp))
  | Recursive ->
    let pats = List.map impls ~f:(fun impl -> impl.pat) in
    let bindings =
      let inner_bindings =
        List.map impls ~f:(fun inner ->
          value_binding
            ~loc:inner.loc
            ~pat:inner.pat
            ~expr:[%expr [%e of_lazy] [%e inner.var]])
      in
      List.map impls ~f:(fun impl ->
        let body = pexp_let ~loc:impl.loc Nonrecursive inner_bindings impl.exp in
        let lazy_expr = [%expr lazy [%e body]] in
        value_binding ~loc:impl.loc ~pat:impl.pat ~expr:lazy_expr)
    in
    let body =
      pexp_tuple
        ~loc
        (List.map impls ~f:(fun impl -> [%expr [%e of_lazy] [%e impl.var]]))
    in
    pstr_value_list
      ~loc
      Nonrecursive
      [ value_binding
          ~loc
          ~pat:(ppat_tuple ~loc pats)
          ~expr:(pexp_let ~loc Recursive bindings body)
      ]
;;

let generator_impl_list decls ~loc ~rec_flag =
  maybe_mutually_recursive
    decls
    ~loc
    ~rec_flag
    ~of_lazy:[%expr Base_quickcheck.Generator.of_lazy]
    ~impl:generator_impl
;;

let observer_impl_list decls ~loc ~rec_flag =
  maybe_mutually_recursive
    decls
    ~loc
    ~rec_flag
    ~of_lazy:[%expr Base_quickcheck.Observer.of_lazy]
    ~impl:observer_impl
;;

let shrinker_impl_list decls ~loc ~rec_flag =
  maybe_mutually_recursive
    decls
    ~loc
    ~rec_flag
    ~of_lazy:[%expr Base_quickcheck.Shrinker.of_lazy]
    ~impl:shrinker_impl
;;

let intf type_decl ~f ~covar ~contravar =
  let covar = Longident.parse ("Base_quickcheck." ^ covar ^ ".t") in
  let contravar = Longident.parse ("Base_quickcheck." ^ contravar ^ ".t") in
  let type_decl = name_type_params_in_td type_decl in
  let loc = type_decl.ptype_loc in
  let name = loc_map type_decl.ptype_name ~f in
  let result =
    ptyp_constr
      ~loc
      { loc; txt = covar }
      [ ptyp_constr
          ~loc
          (lident_loc type_decl.ptype_name)
          (List.map type_decl.ptype_params ~f:fst)
      ]
  in
  let type_ =
    List.fold_right
      type_decl.ptype_params
      ~init:result
      ~f:(fun (core_type, (variance, injectivity)) result ->
        let id =
          match (variance, injectivity) with
          | ((NoVariance | Covariant), NoInjectivity) -> covar
          | (Contravariant, NoInjectivity) -> contravar
          | (_, Injective) -> Location.raise_errorf ~loc "Injective type parameters aren't supported."
        in
        let arg = ptyp_constr ~loc { loc; txt = id } [ core_type ] in
        [%type: [%t arg] -> [%t result]])
  in
  psig_value ~loc (value_description ~loc ~name ~type_ ~prim:[])
;;

let shrinker_intf = intf ~f:shrinker_name ~covar:"Shrinker" ~contravar:"Shrinker"
let generator_intf = intf ~f:generator_name ~covar:"Generator" ~contravar:"Observer"
let observer_intf = intf ~f:observer_name ~covar:"Observer" ~contravar:"Generator"
let generator_intf_list type_decl_list = List.map type_decl_list ~f:generator_intf
let observer_intf_list type_decl_list = List.map type_decl_list ~f:observer_intf
let shrinker_intf_list type_decl_list = List.map type_decl_list ~f:shrinker_intf

let sig_type_decl =
  Deriving.Generator.make_noarg (fun ~loc:_ ~path:_ (_, decls) ->
    generator_intf_list decls @ observer_intf_list decls @ shrinker_intf_list decls)
;;

let str_type_decl =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (rec_flag, decls) ->
    let rec_flag = really_recursive rec_flag decls in
    generator_impl_list ~loc ~rec_flag decls
    @ observer_impl_list ~loc ~rec_flag decls
    @ shrinker_impl_list ~loc ~rec_flag decls)
;;

let generator_extension ~loc:_ ~path:_ core_type =
  generator_of_core_type core_type ~gen_env:Environment.empty ~obs_env:Environment.empty
;;

let observer_extension ~loc:_ ~path:_ core_type =
  observer_of_core_type core_type ~obs_env:Environment.empty ~gen_env:Environment.empty
;;

let shrinker_extension ~loc:_ ~path:_ core_type =
  shrinker_of_core_type core_type ~env:Environment.empty
;;
