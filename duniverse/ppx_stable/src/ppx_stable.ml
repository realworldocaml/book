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
    (module_binding
       ~loc
       ~name:(Located.mk ~loc (Some name))
       ~expr:(pmod_structure ~loc items))
;;

(* fun ~name:name -> exp *)
let mk_pexp_fun ~loc ~name exp = pexp_fun ~loc (Labelled name) None (pvar ~loc name) exp
let recurse_name = "recurse"

let map_if_recursive ~loc ~rec_flag type_name =
  match rec_flag with
  | Recursive -> Map.singleton (module String) type_name (evar ~loc recurse_name)
  | Nonrecursive -> Map.empty (module String)
;;

(* we only need to mark the function recursive if we made any recursive calls. the type
   might say recursive (the default) without actually being recursive *)
let set_any_recursive_and_return_expr ~any_recursive (result, expr) =
  match (result : Generic_map.replace_result) with
  | Unchanged -> expr
  | Replaced ->
    any_recursive := true;
    expr
;;

(* This is complicated so here's some help on fields:
   - source = fields from record [@@deriving stable_record] attached to
   - target = source + add - remove
   - add:[ a ] = value is gong to come from ~a argument
   - set:[ s ] = value is going to come from ~s argument
   - modify:[ m ] = value is going to come from ~modify_m argument

   In particular:
   fields_from_args = set + (target - source) = set + (add - remove)
   are the fields that we expect to get from ~a and ~s arguments.
*)
let convert_record
      ~loc
      ~fields
      ~source_fields
      ~target_fields
      ~modified_fields
      ~set_fields
      ~source_type
      ~target_type
      ~rec_flag
      ~type_name
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
  let fields_from_args = Set.union set_fields (Set.diff target_fields source_fields) in
  let any_recursive = ref false in
  let fields =
    Map.of_alist_exn (module String) (List.map fields ~f:(fun ld -> ld.pld_name.txt, ld))
  in
  let map_if_recursive = map_if_recursive ~loc ~rec_flag type_name in
  let target_record =
    let fields =
      List.map (Set.to_list target_fields) ~f:(fun name ->
        let expr =
          if Set.mem modified_fields name
          then (
            let f = evar ~loc (modify_field_name name) in
            pexp_apply ~loc f [ Nolabel, evar ~loc name ])
          else if Set.mem fields_from_args name
          then evar ~loc name
          else (
            let ld = Map.find_exn fields name in
            Generic_map.build ~loc ~map:map_if_recursive ld.pld_type (evar ~loc name)
            |> set_any_recursive_and_return_expr ~any_recursive)
        in
        mk_lident ~loc name, expr)
    in
    pexp_record ~loc fields None
  in
  let acc =
    match !any_recursive with
    | false ->
      [%expr
        let ([%p record_pat] : [%t source_type]) = _t in
        ([%e target_record] : [%t target_type])]
    | true ->
      [%expr
        let rec [%p pvar ~loc recurse_name] =
          fun ([%p record_pat] : [%t source_type]) : [%t target_type] -> [%e target_record]
        in
        [%e evar ~loc recurse_name] _t]
  in
  let acc =
    Set.fold fields_from_args ~init:acc ~f:(fun acc name -> mk_pexp_fun ~loc ~name acc)
  in
  let acc =
    Set.fold_right modified_fields ~init:acc ~f:(fun name acc ->
      let name = modify_field_name name in
      mk_pexp_fun ~loc ~name acc)
  in
  (* we put this argument first to help with record field disambiguation at the use site *)
  [%expr fun (_t : [%t source_type]) -> [%e acc]]
;;

let cd_args_and_value ~loc ~tuple_opt ~record ~f cd =
  match cd.pcd_args with
  | Pcstr_tuple tys ->
    let args, pats =
      List.mapi tys ~f:(fun i ty ->
        let var = "v" ^ Int.to_string i in
        (Nolabel, var), f ty var)
      |> List.unzip
    in
    args, tuple_opt pats
  | Pcstr_record lds ->
    let args, pats =
      List.mapi lds ~f:(fun i ld ->
        let var = "v" ^ Int.to_string i in
        ( (Labelled ld.pld_name.txt, var)
        , (Located.lident ~loc ld.pld_name.txt, f ld.pld_type var) ))
      |> List.unzip
    in
    args, Some (record pats)
;;

let generate_stable_variant_module ~td ~loc ~cdl =
  let alias_fun_label cd = String.lowercase cd.pcd_name.txt ^ "_fun" in
  let map_function =
    let cases =
      List.map cdl ~f:(fun cd ->
        let args, pattern =
          cd_args_and_value
            cd
            ~loc
            ~tuple_opt:(ppat_tuple_opt ~loc)
            ~record:(fun p -> ppat_record ~loc p Closed)
            ~f:(fun _ x -> pvar ~loc x)
        in
        let pattern =
          ppat_construct ~loc (Located.lident ~loc cd.pcd_name.txt) pattern
        in
        let value =
          let fun_expr = evar ~loc (alias_fun_label cd) in
          if List.is_empty args
          then [%expr [%e fun_expr] ()]
          else
            List.map args ~f:(fun (lbl, x) -> lbl, evar ~loc x)
            |> pexp_apply ~loc fun_expr
        in
        case ~guard:None ~lhs:pattern ~rhs:value)
    in
    let expr =
      List.fold_right ~init:(pexp_function ~loc cases) cdl ~f:(fun cd acc ->
        let name = String.lowercase cd.pcd_name.txt in
        pexp_fun ~loc (Labelled name) None (pvar ~loc (alias_fun_label cd)) acc)
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
      ~items:[ map_function ]
  ]
;;

let convert_variant
      ~loc
      ~constructors
      ~source_variants
      ~target_variants
      ~modified_variants
      ~target_type
      ~source_type
      ~rec_flag
      ~type_name
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
    Located.mk ~loc longident
  in
  let constructors =
    Map.of_alist_exn
      (module String)
      (List.map constructors ~f:(fun cd -> cd.pcd_name.txt, cd))
  in
  let any_recursive = ref false in
  let map_if_recursive = map_if_recursive ~loc ~rec_flag type_name in
  let map_cd cd =
    let args, value =
      cd_args_and_value
        cd
        ~loc
        ~tuple_opt:(pexp_tuple_opt ~loc)
        ~record:(fun e -> pexp_record ~loc e None)
        ~f:(fun ty alias ->
          Generic_map.build ~loc ~map:map_if_recursive ty (evar ~loc alias)
          |> set_any_recursive_and_return_expr ~any_recursive)
    in
    let value =
      pexp_construct
        ~loc
        (variants_longident ~which_type:target_type ~loc [ cd.pcd_name.txt ])
        value
    in
    if List.is_empty args
    then [%expr fun () -> [%e value]]
    else
      List.fold_right args ~init:value ~f:(fun (label, alias) acc ->
        pexp_fun ~loc label None (pvar ~loc alias) acc)
  in
  let acc =
    let map_fn =
      variants_longident
        ~loc
        ~which_type:source_type
        [ stable_variants ~type_:source_type; "Helper"; "map" ]
    in
    [%expr [%e pexp_ident ~loc map_fn] v]
  in
  let rhs =
    Set.fold source_variants ~init:acc ~f:(fun acc name ->
      let f =
        if Set.mem modified_variants name
        then evar ~loc (modify_field_name name)
        else if not (Set.mem target_variants name)
        then evar ~loc (remove_field_name name)
        else map_cd (Map.find_exn constructors name)
      in
      pexp_apply ~loc acc [ Labelled (String.lowercase name), f ])
  in
  let acc =
    match !any_recursive with
    | false -> [%expr fun (v : [%t source_type]) : [%t target_type] -> [%e rhs]]
    | true ->
      [%expr
        let rec [%p pvar ~loc recurse_name] =
          fun (v : [%t source_type]) : [%t target_type] -> [%e rhs]
        in
        [%e evar ~loc recurse_name]]
  in
  let acc =
    Set.fold (Set.diff source_variants target_variants) ~init:acc ~f:(fun acc name ->
      let name = remove_field_name name in
      mk_pexp_fun ~loc ~name acc)
  in
  let acc =
    Set.fold_right modified_variants ~init:acc ~f:(fun name acc ->
      let name = modify_field_name name in
      mk_pexp_fun ~loc ~name acc)
  in
  acc
;;

module Changes_by_type = struct
  type 'a t =
    { add : 'a
    ; modify : 'a
    ; set : 'a
    ; remove : 'a
    }

  type kind =
    | Add
    | Modify
    | Set
    | Remove

  let set t kind value =
    match kind with
    | Add -> { t with add = value }
    | Modify -> { t with modify = value }
    | Set -> { t with set = value }
    | Remove -> { t with remove = value }
  ;;

  let get t kind =
    match kind with
    | Add -> t.add
    | Modify -> t.modify
    | Set -> t.set
    | Remove -> t.remove
  ;;

  let create x = { add = x; modify = x; set = x; remove = x }

  let map t ~f =
    { add = f t.add; modify = f t.modify; set = f t.set; remove = f t.remove }
  ;;

  let to_list t = [ t.add; t.modify; t.set; t.remove ]
end

let conversions_of_td ~ppx_name ~target_type ~rec_flag changes td =
  let ({ add; modify; set; remove } : _ Changes_by_type.t) = changes in
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
              ~fields:lds
              ~source_fields:current_fields
              ~target_fields:other_fields
              ~modified_fields:modify
              ~set_fields:set
              ~target_type
              ~source_type:current_type
              ~rec_flag
              ~type_name:td.ptype_name.txt
          in
          let of_target =
            convert_record
              ~loc
              ~fields:lds
              ~source_fields:other_fields
              ~target_fields:current_fields
              ~modified_fields:modify
              ~set_fields:set
              ~target_type:current_type
              ~source_type:target_type
              ~rec_flag
              ~type_name:td.ptype_name.txt
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
              ~constructors:cdl
              ~source_variants:current_variants
              ~target_variants:other_variants
              ~modified_variants:modify
              ~target_type
              ~source_type:current_type
              ~rec_flag
              ~type_name:td.ptype_name.txt
          in
          let of_target =
            convert_variant
              ~loc
              ~constructors:cdl
              ~source_variants:other_variants
              ~target_variants:current_variants
              ~modified_variants:modify
              ~target_type:current_type
              ~source_type:target_type
              ~rec_flag
              ~type_name:td.ptype_name.txt
          in
          to_target, of_target
        | Ptype_open -> Location.raise_errorf ~loc "%s: open types not supported" ppx_name
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

let fields_or_constructors () =
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

let stable_changes =
  let raise_invalid_change_argument ~loc =
    Location.raise_errorf
      ~loc
      "Invalid change argument. Expected add, modify, set, or remove."
  in
  Attribute.declare
    "stable.changes"
    Type_declaration
    Ast_pattern.(pstr (pstr_eval (pexp_apply (estring (string "")) __) nil ^:: nil))
    (fun args : _ Changes_by_type.t ->
       let init = Changes_by_type.create None in
       List.fold args ~init ~f:(fun acc (label, expression) ->
         let loc = expression.pexp_loc in
         let name =
           match label with
           | Labelled name -> name
           | Nolabel | Optional _ -> raise_invalid_change_argument ~loc
         in
         let kind : Changes_by_type.kind =
           match name with
           | "add" -> Add
           | "modify" -> Modify
           | "set" -> Set
           | "remove" -> Remove
           | _ -> raise_invalid_change_argument ~loc
         in
         let value =
           Ast_pattern.parse (fields_or_constructors ()) loc expression Fn.id
         in
         match Changes_by_type.get acc kind with
         | None -> Changes_by_type.set acc kind (Some value)
         | Some _ -> Location.raise_errorf ~loc "%s argument was passed twice" name)
       |> Changes_by_type.map ~f:(Option.value ~default:[]))
;;

let make_stable_changes_attribute
      ~loc
      ?(add = [])
      ?(modify = [])
      ?(set = [])
      ?(remove = [])
      ()
  =
  let open (val Ast_builder.make loc) in
  let mkident x =
    if Char.is_lowercase x.[0]
    then pexp_ident (Located.lident x)
    else pexp_construct (Located.lident x) None
  in
  let ident_list names = elist (List.map ~f:mkident names) in
  let change_expression =
    pexp_apply
      [%expr ""]
      [ Labelled "add", ident_list add
      ; Labelled "set", ident_list set
      ; Labelled "modify", ident_list modify
      ; Labelled "remove", ident_list remove
      ]
  in
  attribute
    ~name:(Located.mk (Attribute.name stable_changes))
    ~payload:(PStr [ pstr_eval change_expression [] ])
;;

let args =
  Deriving.Args.(
    let changes = pack2 (pexp_loc __ (fields_or_constructors ())) in
    empty
    +> arg "version" type_pattern
    +> arg "add" changes
    +> arg "modify" changes
    +> arg "set" changes
    +> arg "remove" changes)
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

let gen ppx_name ~loc ~path:_ (rec_flag, tds) target_type add modify set remove =
  match tds with
  | [ td ] ->
    let changes_from_args : _ Changes_by_type.t = { add; modify; set; remove } in
    let changes =
      match Attribute.get stable_changes td with
      | Some changes_from_attribute ->
        (match Changes_by_type.to_list changes_from_args |> List.find_map ~f:Fn.id with
         | None -> ()
         | Some (loc, _) ->
           Location.raise_errorf
             ~loc
             "The changes (add, modify, set, or remove) passed to\n\
              [@@@@deriving %s] are unnecessary. They are already\n\
              specified by the [@@@@stable.changes] attribute."
             ppx_name);
        changes_from_attribute
      | None ->
        Changes_by_type.map changes_from_args ~f:(Option.value_map ~f:snd ~default:[])
    in
    conversions_of_td ~rec_flag ~ppx_name ~target_type changes td
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
