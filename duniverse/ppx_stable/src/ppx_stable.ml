open Base
open Ppxlib
open Ast_builder.Default

module Invariant = struct
  let set_to_string set =
    set
    |> Set.to_list
    |> List.map ~f:(fun field_name -> Printf.sprintf "`%s'" field_name)
    |> String.concat ~sep:", "
  ;;

  let all_disjoints ~loc ~add ~remove ~modify ~set =
    let check (n1, s1) (n2, s2) =
      let common = Set.inter s1 s2 in
      if not (Set.is_empty common)
      then
        Location.raise_errorf
          ~loc
          "Sets `%s' and `%s' must be disjoint but they are not: %s found in both"
          n1
          n2
          (set_to_string common)
    in
    let a = "add", add in
    let b = "remove", remove in
    let c = "modify", modify in
    let d = "set", set in
    check a b;
    check a c;
    check a d;
    check b c;
    check b d;
    check c d
  ;;

  let things_are_known ~loc ~all ~thing_name ~supposed_to_be to_remove =
    let unknown_fields = Set.diff to_remove all in
    if not (Set.is_empty unknown_fields)
    then (
      let str = set_to_string unknown_fields in
      Location.raise_errorf
        ~loc
        "Some %s were supposed to be %s but they were not found: %s"
        thing_name
        supposed_to_be
        str)
  ;;
end

(* {1 Some helper functions} *)

let name_of_type_name ~dir ~source ~type_name =
  match type_name.ptyp_desc with
  | Ptyp_constr ({ txt = type_name; _ }, _) ->
    let fun_name =
      Printf.sprintf
        "%s_%s"
        (match dir with
         | `To -> "to"
         | `Of -> "of")
        (String.concat ~sep:"_" (Longident.flatten_exn type_name))
    in
    (match source with
     | "t" -> fun_name
     | _ -> source ^ "_" ^ fun_name)
  | _ -> assert false
;;

let stable_variant_name ~type_name =
  match type_name with
  | "t" -> "Stable_variant"
  | _ -> "Stable_variant_of_" ^ type_name
;;

let stable_variants ~type_ =
  match type_.ptyp_desc with
  | Ptyp_constr ({ txt = type_; _ }, _) ->
    let type_name = List.hd_exn (List.rev (Longident.flatten_exn type_)) in
    stable_variant_name ~type_name
  | _ -> assert false
;;

let modify_field_name name = "modify_" ^ name
let remove_field_name name = "remove_" ^ name
let mk_lident ~loc str = Located.mk ~loc (Longident.Lident str)

let mk_module ~loc ~name ~items =
  pstr_module
    ~loc
    (module_binding ~loc ~name:(Located.mk ~loc (Some name)) ~expr:(pmod_structure ~loc items))
;;

(* fun ~name:name -> exp *)
let mk_pexp_fun ~loc ~name exp = pexp_fun ~loc (Labelled name) None (pvar ~loc name) exp

let convert_record
      ~loc
      ~source_fields
      ~target_fields
      ~modified_fields
      ~set_fields
      ~source_type
      ~target_type
  =
  let record_pat =
    let record_pat =
      List.map (Set.to_list source_fields) ~f:(fun name ->
        if Set.mem target_fields name && not (Set.mem set_fields name)
        then mk_lident ~loc name, ppat_var ~loc (Located.mk ~loc name)
        else mk_lident ~loc name, ppat_any ~loc)
    in
    ppat_record ~loc record_pat Closed
  in
  let rhs_record =
    let rhs_fields =
      List.map (Set.to_list target_fields) ~f:(fun name ->
        if Set.mem modified_fields name
        then (
          let f = evar ~loc (modify_field_name name) in
          let application = pexp_apply ~loc f [ Asttypes.Nolabel, evar ~loc name ] in
          mk_lident ~loc name, application)
        else (
          let longident = mk_lident ~loc name in
          let expr = pexp_ident ~loc longident in
          longident, expr))
    in
    pexp_record ~loc rhs_fields None
  in
  let acc = [%expr ([%e rhs_record] : [%t target_type])] in
  let acc =
    Set.fold
      (Set.union set_fields (Set.diff target_fields source_fields))
      ~init:acc
      ~f:(fun acc name -> mk_pexp_fun ~loc ~name acc)
  in
  let acc =
    Set.fold_right modified_fields ~init:acc ~f:(fun name acc ->
      let name = modify_field_name name in
      mk_pexp_fun ~loc ~name acc)
  in
  [%expr fun ([%p record_pat] : [%t source_type]) -> [%e acc]]
;;

let generate_stable_variant_module ~td ~loc ~cdl =
  let alias_args cd =
    match cd.pcd_args with
    | Pcstr_record lds -> List.mapi lds ~f:(fun i _ -> "v" ^ Int.to_string i)
    | Pcstr_tuple fields -> List.mapi fields ~f:(fun i _ -> "v" ^ Int.to_string i)
  in
  let alias_fun_label name = name ^ "_fun" in
  let labels_and_aliases cd =
    match cd.pcd_args with
    | Pcstr_tuple _ -> List.map (alias_args cd) ~f:(fun alias -> Nolabel, alias)
    | Pcstr_record lds ->
      List.map2_exn (alias_args cd) lds ~f:(fun alias ld ->
        Labelled ld.pld_name.txt, alias)
  in
  let constructors =
    List.map cdl ~f:(fun cd ->
      let arg_aliases = alias_args cd in
      let expr =
        let constructed_value =
          let arg =
            match cd.pcd_args with
            | Pcstr_tuple _ ->
              pexp_tuple_opt
                ~loc
                (List.map arg_aliases ~f:(fun alias -> evar ~loc alias))
            | Pcstr_record lds ->
              Some
                (pexp_record
                   ~loc
                   (List.map2_exn arg_aliases lds ~f:(fun alias ld ->
                      Located.lident ~loc ld.pld_name.txt, evar ~loc alias))
                   None)
          in
          pexp_construct ~loc (Located.lident ~loc cd.pcd_name.txt) arg
        in
        if List.is_empty arg_aliases
        then [%expr fun () -> [%e constructed_value]]
        else
          List.fold_right
            (labels_and_aliases cd)
            ~init:constructed_value
            ~f:(fun (label, alias) acc ->
              pexp_fun ~loc label None (pvar ~loc alias) acc)
      in
      pstr_value
        ~loc
        Nonrecursive
        [ value_binding ~loc ~pat:(pvar ~loc (String.lowercase cd.pcd_name.txt)) ~expr ])
  in
  let map_function =
    let cases =
      List.map cdl ~f:(fun cd ->
        let lowercase = String.lowercase cd.pcd_name.txt in
        let arg_aliases = alias_args cd in
        let pattern =
          let arg =
            match cd.pcd_args with
            | Pcstr_tuple _ ->
              ppat_tuple_opt
                ~loc
                (List.map arg_aliases ~f:(fun alias -> pvar ~loc alias))
            | Pcstr_record lds ->
              Some
                (ppat_record
                   ~loc
                   (List.map2_exn lds arg_aliases ~f:(fun ld alias ->
                      Located.lident ~loc ld.pld_name.txt, pvar ~loc alias))
                   Closed)
          in
          ppat_construct ~loc (Located.lident ~loc cd.pcd_name.txt) arg
        in
        let value =
          let fun_expr = evar ~loc (alias_fun_label lowercase) in
          if List.is_empty arg_aliases
          then [%expr [%e fun_expr] ()]
          else
            pexp_apply
              ~loc
              fun_expr
              (List.map (labels_and_aliases cd) ~f:(fun (lbl, alias) ->
                 lbl, evar ~loc alias))
        in
        case ~guard:None ~lhs:pattern ~rhs:value)
    in
    let expr =
      List.fold_right ~init:(pexp_function ~loc cases) cdl ~f:(fun cd acc ->
        let name = String.lowercase cd.pcd_name.txt in
        pexp_fun ~loc (Labelled name) None (pvar ~loc (alias_fun_label name)) acc)
    in
    mk_module
      ~loc
      ~name:"Helper"
      ~items:
        [ pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:(pvar ~loc "map") ~expr ]
        ]
  in
  [ mk_module
      ~loc
      ~name:(stable_variant_name ~type_name:td.ptype_name.txt)
      ~items:(constructors @ [ map_function ])
  ]
;;

let convert_variant
      ~loc
      ~source_variants
      ~target_variants
      ~modified_variants
      ~target_type
      ~source_type
  =
  (* Create pexp_ident scoped to the same module as [which_type]. *)
  let variants_longident ~loc ~which_type path =
    let add longident_opt x =
      match longident_opt with
      | Some l -> Some (Ldot (l, x))
      | None -> Some (Lident x)
    in
    let init =
      match which_type.ptyp_desc with
      | Ptyp_constr (lid_loc, _) ->
        (match lid_loc.txt with
         | Lapply _ -> Location.raise_errorf ~loc "Unexpected Lapply"
         | Lident _ -> None
         | Ldot (t, _) -> Some t)
      | _ -> assert false
    in
    let longident = Option.value_exn (List.fold ~init path ~f:add) in
    pexp_ident ~loc (Located.mk ~loc longident)
  in
  let acc =
    [%expr
      [%e
        variants_longident
          ~loc
          ~which_type:source_type
          [ stable_variants ~type_:source_type; "Helper"; "map" ]]
        v]
  in
  let rhs =
    Set.fold source_variants ~init:acc ~f:(fun acc name ->
      let f =
        if Set.mem modified_variants name
        then evar ~loc (modify_field_name name)
        else if not (Set.mem target_variants name)
        then evar ~loc (remove_field_name name)
        else
          variants_longident
            ~loc
            ~which_type:target_type
            [ stable_variants ~type_:target_type; String.lowercase name ]
      in
      pexp_apply ~loc acc [ Labelled (String.lowercase name), f ])
  in
  let acc = [%expr fun (v : [%t source_type]) -> ([%e rhs] : [%t target_type])] in
  let acc =
    Set.fold (Set.diff source_variants target_variants) ~init:acc ~f:(fun acc name ->
      let name = remove_field_name name in
      mk_pexp_fun ~loc ~name acc)
  in
  Set.fold_right modified_variants ~init:acc ~f:(fun name acc ->
    let name = modify_field_name name in
    mk_pexp_fun ~loc ~name acc)
;;

let conversions_of_td
      ~ppx_name
      ~target_type
      ?(add = [])
      ?(remove = [])
      ?(modify = [])
      ?(set = [])
      td
  =
  let loc = td.ptype_loc in
  let add = Set.of_list (module String) add in
  let modify = Set.of_list (module String) modify in
  let remove = Set.of_list (module String) remove in
  let set = Set.of_list (module String) set in
  Invariant.all_disjoints ~loc ~add ~modify ~remove ~set;
  let current_type =
    Ast_helper.Typ.constr
      ~loc
      (Located.map_lident td.ptype_name)
      (List.map ~f:fst td.ptype_params)
  in
  let conversions =
    match target_type with
    | None -> []
    | Some target_type ->
      let to_target_name =
        name_of_type_name ~dir:`To ~source:td.ptype_name.txt ~type_name:target_type
      in
      let of_target_name =
        name_of_type_name ~dir:`Of ~source:td.ptype_name.txt ~type_name:target_type
      in
      let to_target, of_target =
        match td.ptype_kind with
        | Ptype_record lds ->
          let current_fields =
            Set.of_list (module String) (List.map lds ~f:(fun ld -> ld.pld_name.txt))
          in
          Invariant.things_are_known
            ~thing_name:"fields"
            ~supposed_to_be:"removed"
            ~loc
            ~all:current_fields
            remove;
          Invariant.things_are_known
            ~thing_name:"fields"
            ~supposed_to_be:"modified"
            ~loc
            ~all:current_fields
            modify;
          Invariant.things_are_known
            ~thing_name:"fields"
            ~supposed_to_be:"set"
            ~loc
            ~all:current_fields
            set;
          let other_fields = Set.diff (Set.union current_fields add) remove in
          let to_target =
            convert_record
              ~loc
              ~source_fields:current_fields
              ~target_fields:other_fields
              ~modified_fields:modify
              ~set_fields:set
              ~target_type
              ~source_type:current_type
          in
          let of_target =
            convert_record
              ~loc
              ~source_fields:other_fields
              ~target_fields:current_fields
              ~modified_fields:modify
              ~set_fields:set
              ~target_type:current_type
              ~source_type:target_type
          in
          to_target, of_target
        | Ptype_variant cdl ->
          let current_variants =
            Set.of_list (module String) (List.map cdl ~f:(fun cd -> cd.pcd_name.txt))
          in
          Invariant.things_are_known
            ~thing_name:"variants"
            ~supposed_to_be:"removed"
            ~loc
            ~all:current_variants
            remove;
          Invariant.things_are_known
            ~thing_name:"variants"
            ~supposed_to_be:"modified"
            ~loc
            ~all:current_variants
            modify;
          if not (Set.is_empty set)
          then Location.raise_errorf ~loc "[set] is for record only";
          let other_variants = Set.diff (Set.union current_variants add) remove in
          let to_target =
            convert_variant
              ~loc
              ~source_variants:current_variants
              ~target_variants:other_variants
              ~modified_variants:modify
              ~target_type
              ~source_type:current_type
          in
          let of_target =
            convert_variant
              ~loc
              ~source_variants:other_variants
              ~target_variants:current_variants
              ~modified_variants:modify
              ~target_type:current_type
              ~source_type:target_type
          in
          to_target, of_target
        | Ptype_open ->
          Location.raise_errorf ~loc "%s: open types not supported" ppx_name
        | Ptype_abstract ->
          Location.raise_errorf ~loc "%s: abstract types not supported" ppx_name
      in
      [ [%stri let [%p pvar ~loc to_target_name] = [%e to_target]]
      ; [%stri let [%p pvar ~loc of_target_name] = [%e of_target]]
      ]
  in
  let extra_struct =
    match td.ptype_kind, target_type with
    | Ptype_variant cdl, _ -> generate_stable_variant_module ~loc ~cdl ~td
    | Ptype_record _, None ->
      Location.raise_errorf ~loc "%s: missing target version" ppx_name
    | Ptype_record _, Some _ | (Ptype_open | Ptype_abstract), _ -> []
  in
  extra_struct @ conversions
;;

let fields_or_constructors =
  let open Ast_pattern in
  let rec_fields_pat = elist (pexp_ident (lident __)) in
  let constrs_pat = elist (pexp_construct (lident __) none) in
  alt rec_fields_pat constrs_pat
;;

let type_pattern =
  let open Ast_pattern in
  let ident =
    map' (pexp_ident __) ~f:(fun loc _ lid ->
      Some (Ast_builder.Default.ptyp_constr ~loc (Located.mk ~loc lid) []))
  in
  let type_ =
    map' (* make sure we get a type constructor. *)
      (pexp_extension (extension (string "stable") (ptyp (ptyp_constr __' __))))
      ~f:(fun loc _ lid params -> Some (Ast_builder.Default.ptyp_constr ~loc lid params))
  in
  alt ident type_
;;

let args =
  Deriving.Args.(
    empty
    +> arg "version" type_pattern
    +> arg "add" fields_or_constructors
    +> arg "modify" fields_or_constructors
    +> arg "set" fields_or_constructors
    +> arg "remove" fields_or_constructors)
;;

(* That's actually useless, it's just here so ppxlib's driver doesn't complain *)
let rewrite_type_ext =
  Extension.declare
    "stable"
    Extension.Context.expression
    Ast_pattern.(ptyp (ptyp_constr __' __))
    (fun ~loc ~path:_ _ _ ->
       [%expr `Do_not_use_percent_stable_outside_of_deriving_stable])
;;

let () = Driver.register_transformation "stable" ~extensions:[ rewrite_type_ext ]

let gen ppx_name ~loc ~path:_ (_rec, tds) target_type add modify set remove =
  match tds with
  | [ td ] -> conversions_of_td ~ppx_name ~target_type ?add ?remove ?modify ?set td
  | _ ->
    Location.raise_errorf
      ~loc
      "mutually recursive types are not supported by ppx_stable_type"
;;

let stable_record =
  let name = "stable_record" in
  let str_type_decl = Deriving.Generator.make args (gen name) in
  Deriving.add name ~str_type_decl
;;

let stable_variant =
  let name = "stable_variant" in
  let str_type_decl = Deriving.Generator.make args (gen name) ~deps:[] in
  Deriving.add name ~str_type_decl
;;
