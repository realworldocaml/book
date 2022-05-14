open! Base
open! Ppxlib
open Ast_builder.Default

let unsupported ~loc string =
  Location.raise_errorf ~loc "sexp_grammar: %s are unsupported" string
;;

let ewith_tag ~loc ~key ~value grammar =
  [%expr { key = [%e key]; value = [%e value]; grammar = [%e grammar] }]
;;

let eno_tag ~loc grammar = [%expr No_tag [%e grammar]]
let etag ~loc with_tag = [%expr Tag [%e with_tag]]
let etagged ~loc with_tag = [%expr Tagged [%e with_tag]]

let tag_of_doc_comment ~loc comment =
  ( [%expr Ppx_sexp_conv_lib.Sexp_grammar.doc_comment_tag]
  , [%expr Atom [%e estring ~loc comment]] )
;;

let with_tags grammar ~f ~loc ~tags ~comments =
  let tags = List.concat [ List.map comments ~f:(tag_of_doc_comment ~loc); tags ] in
  List.fold_right tags ~init:grammar ~f:(fun (key, value) grammar ->
    f ~loc (ewith_tag ~loc ~key ~value grammar))
;;

let with_tags_as_list grammar ~loc ~tags ~comments =
  with_tags (eno_tag ~loc grammar) ~f:etag ~loc ~tags ~comments
;;

let with_tags_as_grammar grammar ~loc ~tags ~comments =
  with_tags grammar ~f:etagged ~loc ~tags ~comments
;;

let grammar_name name = name ^ "_sexp_grammar"
let tyvar_grammar_name name = grammar_name ("_'" ^ name)
let estr { loc; txt } = estring ~loc txt
let grammar_type ~loc core_type = [%type: [%t core_type] Sexplib0.Sexp_grammar.t]

let abstract_grammar ~ctxt ~loc id =
  let module_name =
    ctxt |> Expansion_context.Deriver.code_path |> Code_path.fully_qualified_path
  in
  [%expr Any [%e estr { id with txt = String.concat ~sep:"." [ module_name; id.txt ] }]]
;;

let arrow_grammar ~loc = [%expr Sexplib0.Sexp_conv.fun_sexp_grammar.untyped]
let opaque_grammar ~loc = [%expr Sexplib0.Sexp_conv.opaque_sexp_grammar.untyped]
let wildcard_grammar ~loc = [%expr Any "_"]
let list_grammar ~loc expr = [%expr List [%e expr]]
let many_grammar ~loc expr = [%expr Many [%e expr]]
let fields_grammar ~loc expr = [%expr Fields [%e expr]]
let tyvar_grammar ~loc expr = [%expr Tyvar [%e expr]]
let tycon_grammar ~loc name args = [%expr Tycon ([%e name], [%e args])]
let recursive_grammar ~loc grammar defns = [%expr Recursive ([%e grammar], [%e defns])]
let defns_type ~loc = [%type: Sexplib0.Sexp_grammar.defn Stdlib.List.t Stdlib.Lazy.t]

let untyped_grammar ~loc expr =
  match expr with
  | [%expr { untyped = [%e? untyped] }] -> untyped
  | _ -> [%expr [%e expr].untyped]
;;

let typed_grammar ~loc expr =
  match expr with
  | [%expr [%e? typed].untyped] -> typed
  | _ -> [%expr { untyped = [%e expr] }]
;;

let defn_expr ~loc ~tycon ~tyvars ~grammar =
  [%expr { tycon = [%e tycon]; tyvars = [%e tyvars]; grammar = [%e grammar] }]
;;

let union_grammar ~loc exprs =
  match exprs with
  | [] -> [%expr Union []]
  | [ expr ] -> expr
  | _ -> [%expr Union [%e elist ~loc exprs]]
;;

let tuple_grammar ~loc exprs =
  List.fold_right exprs ~init:[%expr Empty] ~f:(fun expr rest ->
    [%expr Cons ([%e expr], [%e rest])])
;;

let atom_clause ~loc = [%expr Atom_clause]
let list_clause ~loc args = [%expr List_clause { args = [%e args] }]

module Variant_clause_type = struct
  type t =
    { name : label loc
    ; comments : string list
    ; tags : (expression * expression) list
    ; clause_kind : expression
    }

  let to_grammar_expr { name; comments; tags; clause_kind } ~loc =
    [%expr { name = [%e estr name]; clause_kind = [%e clause_kind] }]
    |> with_tags_as_list ~loc:name.loc ~comments ~tags
  ;;
end

let variant_grammars ~loc ~case_sensitivity ~clauses =
  match List.is_empty clauses with
  | true -> []
  | false ->
    let clause_exprs = List.map clauses ~f:(Variant_clause_type.to_grammar_expr ~loc) in
    let grammar =
      [%expr
        Variant
          { case_sensitivity = [%e case_sensitivity]
          ; clauses = [%e elist ~loc clause_exprs]
          }]
    in
    [ grammar ]
;;

(* Wrap [expr] in [fun a b ... ->] for type parameters. *)
let td_params_fun td expr =
  let loc = td.ptype_loc in
  let params =
    List.map td.ptype_params ~f:(fun param ->
      let { loc; txt } = get_type_param_name param in
      pvar ~loc (tyvar_grammar_name txt))
  in
  eabstract ~loc params expr
;;

module Row_field_type = struct
  type t =
    | Inherit of core_type
    | Tag_no_arg of string loc
    | Tag_with_arg of string loc * core_type

  let of_row_field ~loc row_field =
    match row_field with
    | Rinherit core_type -> Inherit core_type
    | Rtag (name, possibly_no_arg, possible_type_args) ->
      (match possibly_no_arg, possible_type_args with
       | true, [] -> Tag_no_arg name
       | false, [ core_type ] -> Tag_with_arg (name, core_type)
       | false, [] -> unsupported ~loc "empty polymorphic variant types"
       | true, _ :: _ | false, _ :: _ :: _ -> unsupported ~loc "intersection types")
  ;;
end

let attr_doc_comments attributes ~tags_of_doc_comments =
  match tags_of_doc_comments with
  | false -> []
  | true ->
    let doc_pattern = Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil)) in
    List.filter_map attributes ~f:(fun attribute ->
      match attribute.attr_name.txt with
      | "ocaml.doc" | "doc" ->
        Ast_pattern.parse
          doc_pattern
          attribute.attr_loc
          attribute.attr_payload
          ~on_error:(fun () -> None)
          (fun doc -> Some doc)
      | _ -> None)
;;

let grammar_of_type_tags core_type grammar ~tags_of_doc_comments =
  let tags = Attribute.get Attrs.tag_type core_type |> Option.value ~default:[] in
  let loc = core_type.ptyp_loc in
  let comments = attr_doc_comments ~tags_of_doc_comments core_type.ptyp_attributes in
  with_tags_as_grammar grammar ~loc ~tags ~comments
;;

let grammar_of_field_tags field grammar ~tags_of_doc_comments =
  let tags = Attribute.get Attrs.tag_ld field |> Option.value ~default:[] in
  let loc = field.pld_loc in
  let comments = attr_doc_comments ~tags_of_doc_comments field.pld_attributes in
  with_tags_as_list grammar ~loc ~tags ~comments
;;

let rec grammar_of_type core_type ~rec_flag ~tags_of_doc_comments =
  let loc = core_type.ptyp_loc in
  let grammar =
    match Attribute.get Attrs.opaque core_type with
    | Some () -> opaque_grammar ~loc
    | None ->
      (match core_type.ptyp_desc with
       | Ptyp_any -> wildcard_grammar ~loc
       | Ptyp_var name ->
         (match rec_flag with
          | Recursive ->
            (* For recursive grammars, [grammar_of_type] for any type variables is called
               inside a [defn]. The variables should therefore be resolved as [Tyvar]
               grammars. *)
            tyvar_grammar ~loc (estring ~loc name)
          | Nonrecursive ->
            (* Outside recursive [defn]s, type variables are passed in as function
               arguments. *)
            unapplied_type_constr_conv ~loc ~f:tyvar_grammar_name (Located.lident ~loc name)
            |> untyped_grammar ~loc)
       | Ptyp_arrow _ -> arrow_grammar ~loc
       | Ptyp_tuple list ->
         List.map ~f:(grammar_of_type ~rec_flag ~tags_of_doc_comments) list
         |> tuple_grammar ~loc
         |> list_grammar ~loc
       | Ptyp_constr (id, args) ->
         List.map args ~f:(fun core_type ->
           let loc = core_type.ptyp_loc in
           grammar_of_type ~rec_flag ~tags_of_doc_comments core_type
           |> typed_grammar ~loc)
         |> type_constr_conv ~loc ~f:grammar_name id
         |> untyped_grammar ~loc
       | Ptyp_object _ -> unsupported ~loc "object types"
       | Ptyp_class _ -> unsupported ~loc "class types"
       | Ptyp_alias _ -> unsupported ~loc "type aliases"
       | Ptyp_variant (rows, closed_flag, (_ : string list option)) ->
         (match closed_flag with
          | Open -> unsupported ~loc "open polymorphic variant types"
          | Closed ->
            grammar_of_polymorphic_variant ~loc ~rec_flag ~tags_of_doc_comments rows)
       | Ptyp_poly _ -> unsupported ~loc "explicitly polymorphic types"
       | Ptyp_package _ -> unsupported ~loc "first-class module types"
       | Ptyp_extension _ -> unsupported ~loc "unexpanded ppx extensions")
  in
  grammar_of_type_tags core_type grammar ~tags_of_doc_comments

and grammar_of_polymorphic_variant ~loc ~rec_flag ~tags_of_doc_comments rows =
  let inherits, clauses =
    List.partition_map rows ~f:(fun row : (_, Variant_clause_type.t) Either.t ->
      let tags = Attribute.get Attrs.tag_poly row |> Option.value ~default:[] in
      let comments = attr_doc_comments ~tags_of_doc_comments row.prf_attributes in
      match Attribute.get Attrs.list_poly row with
      | Some () ->
        (match Row_field_type.of_row_field ~loc row.prf_desc with
         | Tag_with_arg (name, [%type: [%t? ty] list]) ->
           let clause_kind =
             grammar_of_type ~rec_flag ~tags_of_doc_comments ty
             |> many_grammar ~loc
             |> list_clause ~loc
           in
           Second { name; comments; tags; clause_kind }
         | _ -> Attrs.invalid_attribute ~loc Attrs.list_poly "_ list")
      | None ->
        (match Row_field_type.of_row_field ~loc row.prf_desc with
         | Inherit core_type ->
           First
             (grammar_of_type ~rec_flag ~tags_of_doc_comments core_type
              |> with_tags_as_grammar ~loc ~tags ~comments)
         | Tag_no_arg name ->
           Second { name; comments; tags; clause_kind = atom_clause ~loc }
         | Tag_with_arg (name, core_type) ->
           let clause_kind =
             [ grammar_of_type ~rec_flag ~tags_of_doc_comments core_type ]
             |> tuple_grammar ~loc
             |> list_clause ~loc
           in
           Second { name; comments; tags; clause_kind }))
  in
  variant_grammars ~loc ~case_sensitivity:[%expr Case_sensitive] ~clauses
  |> List.append inherits
  |> union_grammar ~loc
;;

let record_expr ~loc ~rec_flag ~tags_of_doc_comments ~extra_attr syntax fields =
  let fields =
    List.map fields ~f:(fun field ->
      let loc = field.pld_loc in
      let field_kind = Record_field_attrs.Of_sexp.create ~loc field in
      let required =
        match field_kind with
        | Specific Required -> true
        | Specific (Default _)
        | Sexp_bool | Sexp_option _ | Sexp_array _ | Sexp_list _ | Omit_nil -> false
      in
      let args =
        match field_kind with
        | Specific Required | Specific (Default _) | Omit_nil ->
          [%expr
            Cons
              ( [%e grammar_of_type ~tags_of_doc_comments ~rec_flag field.pld_type]
              , Empty )]
        | Sexp_bool -> [%expr Empty]
        | Sexp_option ty ->
          [%expr Cons ([%e grammar_of_type ~tags_of_doc_comments ~rec_flag ty], Empty)]
        | Sexp_list ty | Sexp_array ty ->
          [%expr
            Cons
              ( List (Many [%e grammar_of_type ~tags_of_doc_comments ~rec_flag ty])
              , Empty )]
      in
      [%expr
        { name = [%e estr field.pld_name]
        ; required = [%e ebool ~loc required]
        ; args = [%e args]
        }]
      |> grammar_of_field_tags field ~tags_of_doc_comments)
  in
  let allow_extra_fields =
    match Attribute.get extra_attr syntax with
    | Some () -> true
    | None -> false
  in
  [%expr
    { allow_extra_fields = [%e ebool ~loc allow_extra_fields]
    ; fields = [%e elist ~loc fields]
    }]
;;

let grammar_of_variant ~loc ~rec_flag ~tags_of_doc_comments clause_decls =
  let clauses =
    List.map clause_decls ~f:(fun clause : Variant_clause_type.t ->
      let loc = clause.pcd_loc in
      let tags = Attribute.get Attrs.tag_cd clause |> Option.value ~default:[] in
      let comments = attr_doc_comments ~tags_of_doc_comments clause.pcd_attributes in
      match Attribute.get Attrs.list_variant clause with
      | Some () ->
        (match clause.pcd_args with
         | Pcstr_tuple [ [%type: [%t? ty] list] ] ->
           let args =
             many_grammar ~loc (grammar_of_type ty ~rec_flag ~tags_of_doc_comments)
           in
           { name = clause.pcd_name
           ; comments
           ; tags
           ; clause_kind = list_clause ~loc args
           }
         | _ -> Attrs.invalid_attribute ~loc Attrs.list_variant "_ list")
      | None ->
        (match clause.pcd_args with
         | Pcstr_tuple [] ->
           { name = clause.pcd_name; comments; tags; clause_kind = atom_clause ~loc }
         | Pcstr_tuple (_ :: _ as args) ->
           let args =
             tuple_grammar
               ~loc
               (List.map args ~f:(grammar_of_type ~rec_flag ~tags_of_doc_comments))
           in
           { name = clause.pcd_name
           ; comments
           ; tags
           ; clause_kind = list_clause ~loc args
           }
         | Pcstr_record fields ->
           let args =
             record_expr
               ~loc
               ~rec_flag
               ~tags_of_doc_comments
               ~extra_attr:Attrs.allow_extra_fields_cd
               clause
               fields
             |> fields_grammar ~loc
           in
           { name = clause.pcd_name
           ; comments
           ; tags
           ; clause_kind = list_clause ~loc args
           }))
  in
  variant_grammars
    ~loc
    ~case_sensitivity:[%expr Case_sensitive_except_first_character]
    ~clauses
  |> union_grammar ~loc
;;

let grammar_of_td ~ctxt ~rec_flag ~tags_of_doc_comments td =
  let loc = td.ptype_loc in
  match td.ptype_kind with
  | Ptype_open -> unsupported ~loc "open types"
  | Ptype_record fields ->
    record_expr
      ~loc
      ~rec_flag
      ~tags_of_doc_comments
      ~extra_attr:Attrs.allow_extra_fields_td
      td
      fields
    |> fields_grammar ~loc
    |> list_grammar ~loc
  | Ptype_variant clauses ->
    grammar_of_variant ~loc ~rec_flag ~tags_of_doc_comments clauses
  | Ptype_abstract ->
    (match td.ptype_manifest with
     | None -> abstract_grammar ~ctxt ~loc td.ptype_name
     | Some core_type -> grammar_of_type ~rec_flag ~tags_of_doc_comments core_type)
;;

let pattern_of_td td =
  let { loc; txt } = td.ptype_name in
  ppat_constraint
    ~loc
    (pvar ~loc (grammar_name txt))
    (combinator_type_of_type_declaration td ~f:grammar_type)
;;

(* Any grammar expression that is purely a constant does no work, and does not need to be
   wrapped in [Lazy]. *)
let rec is_preallocated_constant expr =
  match expr.pexp_desc with
  | Pexp_constraint (expr, _) | Pexp_coerce (expr, _, _) | Pexp_open (_, expr) ->
    is_preallocated_constant expr
  | Pexp_constant _ -> true
  | Pexp_tuple args -> List.for_all ~f:is_preallocated_constant args
  | Pexp_variant (_, maybe_arg) | Pexp_construct (_, maybe_arg) ->
    Option.for_all ~f:is_preallocated_constant maybe_arg
  | Pexp_record (fields, maybe_template) ->
    List.for_all fields ~f:(fun (_, expr) -> is_preallocated_constant expr)
    && Option.for_all ~f:is_preallocated_constant maybe_template
  | _ -> false
;;

(* Any grammar expression that just refers to a previously defined grammar also does not
   need to be wrapped in [Lazy]. Accessing the previous grammar is work, but building the
   closure for a lazy value is at least as much work anyway. *)
let rec is_variable_access expr =
  match expr.pexp_desc with
  | Pexp_constraint (expr, _) | Pexp_coerce (expr, _, _) | Pexp_open (_, expr) ->
    is_variable_access expr
  | Pexp_ident _ -> true
  | Pexp_field (expr, _) -> is_variable_access expr
  | _ -> false
;;

let grammar_needs_lazy_wrapper expr =
  not (is_preallocated_constant expr || is_variable_access expr)
;;

let lazy_grammar ~loc td expr =
  if List.is_empty td.ptype_params
  (* polymorphic types generate functions, so the body does not need a [lazy] wrapper *)
  && grammar_needs_lazy_wrapper expr
  then [%expr Lazy (lazy [%e expr])]
  else expr
;;

let force_expr ~loc expr = [%expr Stdlib.Lazy.force [%e expr]]

(* Definitions of grammars that do not refer to each other. *)
let nonrecursive_grammars ~ctxt ~loc ~tags_of_doc_comments td_lists =
  List.concat_map td_lists ~f:(fun tds ->
    List.map tds ~f:(fun td ->
      let td = name_type_params_in_td td in
      let loc = td.ptype_loc in
      let pat = pattern_of_td td in
      let expr =
        grammar_of_td ~ctxt ~rec_flag:Nonrecursive ~tags_of_doc_comments td
        |> lazy_grammar td ~loc
        |> typed_grammar ~loc
        |> td_params_fun td
      in
      value_binding ~loc ~pat ~expr)
    |> pstr_value_list ~loc Nonrecursive)
;;

(* Type constructor grammars used to "tie the knot" for (mutally) recursive grammars. *)
let recursive_grammar_tycons tds =
  List.map tds ~f:(fun td ->
    let td = name_type_params_in_td td in
    let loc = td.ptype_loc in
    let pat = pattern_of_td td in
    let expr =
      tycon_grammar
        ~loc
        (estr td.ptype_name)
        (List.map td.ptype_params ~f:(fun param ->
           let { loc; txt } = get_type_param_name param in
           tyvar_grammar_name txt |> evar ~loc |> untyped_grammar ~loc)
         |> elist ~loc)
      |> typed_grammar ~loc
      |> td_params_fun td
    in
    value_binding ~loc ~pat ~expr)
;;

(* Recursive grammar definitions, based on the type constructors from above. *)
let recursive_grammar_defns ~ctxt ~loc ~tags_of_doc_comments tds =
  List.map tds ~f:(fun td ->
    let td = name_type_params_in_td td in
    let loc = td.ptype_loc in
    let tycon = estr td.ptype_name in
    let tyvars =
      List.map td.ptype_params ~f:(fun param -> estr (get_type_param_name param))
      |> elist ~loc
    in
    let grammar = grammar_of_td ~ctxt ~rec_flag:Recursive ~tags_of_doc_comments td in
    defn_expr ~loc ~tycon ~tyvars ~grammar)
  |> elist ~loc
;;

(* Grammar expression using [Recursive] and a shared definition of grammar definitions.
   The shared definitions are wrapped in [lazy] to avoid toplevel side effects. *)
let recursive_grammar_expr ~defns_name td =
  let td = name_type_params_in_td td in
  let loc = td.ptype_loc in
  let pat = pattern_of_td td in
  let expr =
    let tyvars =
      List.map td.ptype_params ~f:(fun param ->
        let { loc; txt } = get_type_param_name param in
        tyvar_grammar_name txt |> evar ~loc |> untyped_grammar ~loc)
      |> elist ~loc
    in
    recursive_grammar
      ~loc
      (tycon_grammar ~loc (estr td.ptype_name) tyvars)
      (evar ~loc defns_name |> force_expr ~loc)
    |> lazy_grammar td ~loc
    |> typed_grammar ~loc
    |> td_params_fun td
  in
  value_binding ~loc ~pat ~expr
;;

(* Puts together recursive grammar definitions from the parts implemented above. *)
let recursive_grammars ~ctxt ~loc ~tags_of_doc_comments tds =
  match List.is_empty tds with
  | true -> []
  | false ->
    let defns_name = gen_symbol ~prefix:"grammars" () in
    let defns_item =
      let expr =
        recursive_grammar_defns ~ctxt ~loc ~tags_of_doc_comments tds
        |> pexp_let ~loc Nonrecursive (recursive_grammar_tycons tds)
        |> pexp_lazy ~loc
      in
      let pat = ppat_constraint ~loc (pvar ~loc defns_name) (defns_type ~loc) in
      pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat ~expr ]
    in
    let grammars_item =
      List.map tds ~f:(recursive_grammar_expr ~defns_name) |> pstr_value ~loc Nonrecursive
    in
    [%str
      include struct
        open struct
          [%%i defns_item]
        end

        [%%i grammars_item]
      end]
;;

let partition_recursive_and_nonrecursive ~rec_flag tds =
  match (rec_flag : rec_flag) with
  | Nonrecursive -> [], [ tds ]
  | Recursive ->
    (* Pulling out non-recursive references repeatedly means we only "tie the knot" for
       variables that actually need it, and we don't have to manually [ignore] the added
       bindings in case they are unused. *)
    let rec loop tds ~acc =
      let obj =
        object
          inherit type_is_recursive Recursive tds
          method recursion td = {<type_names = [ td.ptype_name.txt ]>}#go ()
        end
      in
      let recursive, nonrecursive =
        List.partition_tf tds ~f:(fun td ->
          match obj#recursion td with
          | Recursive -> true
          | Nonrecursive -> false)
      in
      if List.is_empty recursive || List.is_empty nonrecursive
      then recursive, nonrecursive :: acc
      else loop recursive ~acc:(nonrecursive :: acc)
    in
    loop tds ~acc:[]
;;

let str_type_decl ~ctxt (rec_flag, tds) tags_of_doc_comments =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let recursive, nonrecursive = partition_recursive_and_nonrecursive ~rec_flag tds in
  [ recursive_grammars ~ctxt ~loc ~tags_of_doc_comments recursive
  ; nonrecursive_grammars ~ctxt ~loc ~tags_of_doc_comments nonrecursive
  ]
  |> List.concat
;;

let sig_type_decl ~ctxt:_ (_rec_flag, tds) =
  List.map tds ~f:(fun td ->
    let loc = td.ptype_loc in
    value_description
      ~loc
      ~name:(Loc.map td.ptype_name ~f:grammar_name)
      ~type_:(combinator_type_of_type_declaration td ~f:grammar_type)
      ~prim:[]
    |> psig_value ~loc)
;;

let extension_loc ~ctxt =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  { loc with loc_ghost = true }
;;

let core_type ~tags_of_doc_comments ~ctxt core_type =
  let loc = extension_loc ~ctxt in
  pexp_constraint
    ~loc
    (core_type
     |> grammar_of_type ~rec_flag:Nonrecursive ~tags_of_doc_comments
     |> typed_grammar ~loc)
    (core_type |> grammar_type ~loc)
  |> Merlin_helpers.hide_expression
;;

let type_extension ~ctxt core_type =
  assert_no_attributes_in#core_type core_type;
  let loc = extension_loc ~ctxt in
  core_type |> grammar_type ~loc
;;
