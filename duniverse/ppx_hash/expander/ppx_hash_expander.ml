open Base
open Ppxlib
open Ast_builder.Default

module Attrs = struct
  let ignore_label_declaration =
    Attribute.declare "hash.ignore"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()
  ;;

  let ignore_core_type =
    Attribute.declare "hash.ignore"
      Attribute.Context.core_type
      Ast_pattern.(pstr nil)
      ()
  ;;

  let no_hashing_label_declaration =
    Attribute.declare "hash.no_hashing"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()
  ;;
end

let str_attributes = [
  Attribute.T Attrs.ignore_core_type;
  Attribute.T Attrs.ignore_label_declaration;
  Attribute.T Attrs.no_hashing_label_declaration;
]

let is_ignored_gen attrs t =
  List.exists attrs ~f:(fun attr -> Option.is_some (Attribute.get attr t))

let core_type_is_ignored ct =
  is_ignored_gen
    [ Attrs.ignore_core_type
    ; Ppx_compare_expander.Compare.Attrs.ignore_core_type
    ]
    ct
;;

let should_ignore_label_declaration ld =
  let warning = "[@hash.no_hashing] is deprecated.  Use [@hash.ignore]." in
  let is_ignored =
    is_ignored_gen
      [ Attrs.ignore_label_declaration
      ; Ppx_compare_expander.Compare.Attrs.ignore_label_declaration
      ]
      ld
    (* Avoid confusing errors with [ { mutable field : (value[@ignore]) } ]
       vs [ { mutable field : value [@ignore] } ] by treating them the same. *)
    || core_type_is_ignored ld.pld_type
  in
  match Attribute.get Attrs.no_hashing_label_declaration ld with
  | None -> (if is_ignored then `ignore else `incorporate), None
  | Some () -> `ignore, Some (attribute_of_warning ld.pld_loc warning)
;;


(* Generate code to compute hash values of type [t] in folding style, following the structure of
   the type.  Incorporate all structure when computing hash values, to maximise hash
   quality. Don't attempt to detect/avoid cycles - just loop. *)

let hash_state_t ~loc =
  [%type: Ppx_hash_lib.Std.Hash.state]

let hash_fold_type ~loc ty =
  [%type: [%t hash_state_t ~loc] -> [%t ty] -> [%t hash_state_t ~loc]]

let hash_type ~loc ty =
  [%type: [%t ty] -> Ppx_hash_lib.Std.Hash.hash_value]

(* [expr] is an expression that doesn't use the [hsv] variable.
   Currently it's there only for documentation value, but conceptually it can be thought
   of as an abstract type *)
type expr = expression

(* Represents an expression that produces a hash value and uses the variable [hsv] in
   a linear way (mixes it in exactly once).
   You can think of it as a body of a function of type [Hash.state -> Hash.state] *)
module Hsv_expr : sig
  type t

  val identity : loc:location -> t

  val invoke_hash_fold_t : loc:location -> hash_fold_t:expr -> t:expr -> t

  val compose : loc:location -> t -> t -> t

  (** the [_unchecked] functions all break abstraction in some way *)
  val of_expression_unchecked : expr -> t

  (** the returned [expression] uses the binding [hsv] bound by [pattern] *)
  val to_expression : loc:location -> t -> (pattern * expression)

  (* [case] is binding a variable that's not [hsv] and uses [hsv] on the rhs
     exactly once *)
  type case
  val pexp_match : loc:location -> expr -> case list -> t

  (* [lhs] should not bind [hsv] *)
  val case : lhs:pattern -> guard:expr option -> rhs:t -> case

  (* [value_binding]s should not bind or use [hsv] *)
  val pexp_let : loc:location -> rec_flag -> value_binding list -> t -> t

  val with_attributes : f:(attribute list -> attribute list) -> t -> t
end = struct
  type t = expression

  type nonrec case = case

  let invoke_hash_fold_t ~loc ~hash_fold_t ~t =
    eapply ~loc
      hash_fold_t [[%expr hsv]; t]

  let identity ~loc = [%expr hsv]

  let compose ~loc a b =
    [%expr
      let hsv = [%e a] in
      [%e b]
    ]

  let to_expression ~loc x = ([%pat? hsv], x)
  let of_expression_unchecked x = x

  let pexp_match = pexp_match
  let case = case
  let pexp_let = pexp_let
  let with_attributes ~f x =
    { x with pexp_attributes = f x.pexp_attributes }
end

let hash_fold_int ~loc i : Hsv_expr.t =
  Hsv_expr.invoke_hash_fold_t ~loc
    ~hash_fold_t:[%expr Ppx_hash_lib.Std.Hash.fold_int]
    ~t:(eint ~loc i)

let special_case_types_named_t = function
  | `hash_fold -> false
  | `hash -> true

let hash_fold_ tn =
  match tn with
  | "t" when special_case_types_named_t `hash_fold -> "hash_fold"
  | _ -> "hash_fold_" ^ tn

let hash_ tn =
  match tn with
  | "t" when special_case_types_named_t `hash -> "hash"
  | _ -> "hash_" ^ tn

(** renames [x] avoiding collision with [type_name] *)
let rigid_type_var ~type_name x =
  let prefix = "rigid_" in
  if String.equal x type_name || String.is_prefix x ~prefix
  then prefix ^ x ^ "_of_type_" ^ type_name
  else x

let make_type_rigid ~type_name =
  let map = object
    inherit Ast_traverse.map as super
    method! core_type ty =
      let ptyp_desc =
        let () = (* making sure [type_name] is the only free type variable *)
          match ty.ptyp_desc with
          | Ptyp_constr (name, _args) ->
            (match name.txt with
             | Ldot _ | Lapply _ -> ()
             | Lident name ->
               if (not (String.equal name type_name)) then
                 Location.raise_errorf ~loc:ty.ptyp_loc
                   "ppx_hash: make_type_rigid: unexpected type %S. expected to only find %S"
                   (string_of_core_type ty)
                   type_name;
               ())
          | _ -> ()
        in
        match ty.ptyp_desc with
        | Ptyp_var s ->
          Ptyp_constr (Located.lident ~loc:ty.ptyp_loc (rigid_type_var ~type_name s), [])
        | desc -> super#core_type_desc desc
      in
      { ty with ptyp_desc }
  end in
  map#core_type

(* The only names we assume to be in scope are [hash_fold_<TY>]
   So we are sure [tp_name] (which start with an [_]) will not capture them. *)
let tp_name n = Printf.sprintf "_hash_fold_%s" n

let with_tuple
      loc
      (value : expr)
      xs
      (f : (expr * core_type) list -> Hsv_expr.t) : Hsv_expr.t  =
  let names = List.mapi ~f:(fun i t -> Printf.sprintf "e%d" i, t) xs in
  let pattern =
    let l = List.map ~f:(fun (n, _) -> pvar ~loc n) names in
    ppat_tuple ~loc l
  in
  let e = f (List.map ~f:(fun (n,t) -> (evar ~loc n, t)) names) in
  let binding  = value_binding ~loc ~pat:pattern ~expr:value in
  Hsv_expr.pexp_let ~loc Nonrecursive [binding] e

let hash_ignore ~loc value =
  Hsv_expr.pexp_let ~loc Nonrecursive
    [value_binding ~loc ~pat:[%pat? _] ~expr:value]
    (Hsv_expr.identity ~loc)

let rec hash_applied ty value =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_constr (name, ta) ->
    let args = List.map ta ~f:(hash_fold_of_ty_fun ~type_constraint:false) in
    Hsv_expr.invoke_hash_fold_t
      ~loc
      ~hash_fold_t:(
        type_constr_conv ~loc name ~f:hash_fold_ args)
      ~t:value
  | _ -> assert false

and hash_fold_of_tuple ~loc tys value =
  with_tuple loc value tys (fun elems1 ->
    List.fold_right elems1 ~init:(Hsv_expr.identity ~loc) ~f:(fun (v, t) (result : Hsv_expr.t) ->
      Hsv_expr.compose ~loc
        (hash_fold_of_ty t v)
        result
    ))

and hash_variant ~loc row_fields value =
  let map = fun row ->
    match row.prf_desc with
    | Rtag ({ txt = cnstr; _ }, true, _) | Rtag ({ txt = cnstr; _ }, _, []) ->
      Hsv_expr.case ~guard:None
        ~lhs:(ppat_variant ~loc cnstr None)
        ~rhs:(hash_fold_int ~loc (Ocaml_common.Btype.hash_variant cnstr))
    | Rtag ({ txt = cnstr; _ }, false, tp :: _) ->
      let v = "_v" in
      let body =
        Hsv_expr.compose ~loc
          (hash_fold_int ~loc (Ocaml_common.Btype.hash_variant cnstr))
          (hash_fold_of_ty tp (evar ~loc v))
      in
      Hsv_expr.case ~guard:None
        ~lhs:(ppat_variant ~loc cnstr (Some (pvar ~loc v)))
        ~rhs:body
    | Rinherit ({ ptyp_desc = Ptyp_constr (id, _); _ } as ty) ->
      (* Generated code from..
         type 'a id = 'a [@@deriving hash]
         type t = [ `a | [ `b ] id ] [@@deriving hash]
         doesn't compile: Also see the "sadly" note in: ppx_compare_expander.ml *)
      let v = "_v" in
      Hsv_expr.case ~guard:None
        ~lhs:(ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v))
        ~rhs:(hash_applied ty (evar ~loc v))
    | Rinherit ty ->
      let s = string_of_core_type ty in
      Location.raise_errorf ~loc "ppx_hash: impossible variant case: %s" s
  in
  Hsv_expr.pexp_match ~loc value (List.map ~f:map row_fields)

and branch_of_sum hsv ~loc cd =
  match cd.pcd_args with
  | Pcstr_tuple [] ->
    let pcnstr = pconstruct cd None in
    Hsv_expr.case ~guard:None ~lhs:pcnstr ~rhs:hsv
  | Pcstr_tuple tps ->
    let ids_ty =
      List.mapi tps ~f:(fun i ty -> (Printf.sprintf "_a%d" i, ty))
    in
    let lpatt = List.map ids_ty ~f:(fun (l,_ty) -> pvar ~loc l) |> ppat_tuple ~loc
    and body =
      List.fold_left ids_ty
        ~init:(Hsv_expr.identity ~loc)
        ~f:(fun expr (l,ty) ->
          Hsv_expr.compose ~loc
            expr
            (hash_fold_of_ty ty (evar ~loc l)))
    in
    Hsv_expr.case ~guard:None
      ~lhs:(pconstruct cd (Some lpatt))
      ~rhs:(
        Hsv_expr.compose ~loc
          hsv
          body)
  | Pcstr_record lds ->
    let arg = "_ir" in
    let pat = pvar ~loc arg in
    let v = evar ~loc arg in
    let body = hash_fold_of_record ~loc lds v in
    Hsv_expr.case ~guard:None
      ~lhs:(pconstruct cd (Some pat))
      ~rhs:(
        Hsv_expr.compose ~loc
          hsv
          body)

and branches_of_sum = function
  | [cd] ->
    (* this is an optimization: we don't need to mix in the constructor tag if the type
       only has one constructor *)
    let loc = cd.pcd_loc in
    [branch_of_sum (Hsv_expr.identity ~loc) ~loc cd]
  | cds ->
    List.mapi cds ~f:(fun i cd ->
      let loc = cd.pcd_loc in
      let hsv = hash_fold_int ~loc i in
      branch_of_sum hsv ~loc cd
    )

and hash_sum ~loc cds value =
  Hsv_expr.pexp_match ~loc value (branches_of_sum cds)

and hash_fold_of_ty ty value =
  let loc = ty.ptyp_loc in
  if core_type_is_ignored ty then
    hash_ignore ~loc value
  else
    match ty.ptyp_desc with
    | Ptyp_constr _ -> hash_applied ty value
    | Ptyp_tuple tys -> hash_fold_of_tuple ~loc tys value
    | Ptyp_var name ->
      Hsv_expr.invoke_hash_fold_t
        ~loc
        ~hash_fold_t:(evar ~loc (tp_name name))
        ~t:value
    | Ptyp_arrow _ ->
      Location.raise_errorf ~loc "ppx_hash: functions can not be hashed."
    | Ptyp_variant (row_fields, Closed, None) ->
      hash_variant ~loc row_fields value
    | _ ->
      let s = string_of_core_type ty in
      Location.raise_errorf ~loc "ppx_hash: unsupported type: %s" s

and hash_fold_of_ty_fun ~type_constraint ty =
  let loc = ty.ptyp_loc in
  let arg = "arg" in
  let maybe_constrained_arg =
    if type_constraint then
      ppat_constraint ~loc (pvar ~loc arg) ty
    else
      pvar ~loc arg
  in
  let hsv_pat, hsv_expr =
    Hsv_expr.to_expression ~loc (
      (hash_fold_of_ty ty (evar ~loc arg)))
  in
  eta_reduce_if_possible
    [%expr
      fun [%p hsv_pat] [%p maybe_constrained_arg] ->
        [%e hsv_expr ]
    ]

and hash_fold_of_record ~loc lds value =
  let is_evar = function
    | { pexp_desc = Pexp_ident _; _ } -> true
    | _                               -> false
  in
  assert (is_evar value);
  List.fold_left lds ~init:(Hsv_expr.identity ~loc) ~f:(fun hsv ld ->
    Hsv_expr.compose ~loc hsv (
      let loc = ld.pld_loc in
      let label = Located.map lident ld.pld_name in
      let should_ignore, should_warn = should_ignore_label_declaration ld in
      let field_handling =
        match ld.pld_mutable, should_ignore with
        | Mutable, `incorporate-> `error "require [@hash.ignore] or [@compare.ignore] on mutable record field"
        | (Mutable | Immutable), `ignore -> `ignore
        | Immutable, `incorporate -> `incorporate
      in
      let hsv =
        match field_handling with
        | `error s -> Location.raise_errorf ~loc "ppx_hash: %s" s
        | `incorporate ->
          hash_fold_of_ty ld.pld_type (pexp_field ~loc value label)
        | `ignore -> Hsv_expr.identity ~loc
      in
      match should_warn with
      | None -> hsv
      | Some attribute ->
        Hsv_expr.with_attributes ~f:(fun attributes -> attribute :: attributes) hsv
    ))

let hash_fold_of_abstract ~loc type_name value =
  let str =
    Printf.sprintf
      "hash called on the type %s, which is abstract in an implementation."
      type_name
  in
  Hsv_expr.of_expression_unchecked (
    [%expr
      let _ = hsv in
      let _ = [%e value] in
      failwith [%e estring ~loc str]
    ])

(** this does not change behavior (keeps the expression side-effect if any),
    but it can make the compiler happy when the expression occurs on the rhs
    of an [let rec] binding. *)
let eta_expand ~loc f =
  [%expr let func = [%e f] in fun x -> func x]

let recognize_simple_type ty = match ty.ptyp_desc with
  | Ptyp_constr(lident, []) -> Some lident
  | _ -> None

let hash_of_ty_fun ~special_case_simple_types ~type_constraint ty =
  let loc = ty.ptyp_loc in
  let arg = "arg" in
  let maybe_constrained_arg =
    if type_constraint then
      ppat_constraint ~loc (pvar ~loc arg) ty
    else
      pvar ~loc arg
  in
  match recognize_simple_type ty with
  | Some lident when special_case_simple_types ->
    unapplied_type_constr_conv ~loc lident ~f:hash_
  | _ ->
    let hsv_pat, hsv_expr =
      Hsv_expr.to_expression ~loc
        (hash_fold_of_ty ty (evar ~loc arg))
    in
    [%expr
      fun [%p maybe_constrained_arg] ->
        Ppx_hash_lib.Std.Hash.get_hash_value (
          let [%p hsv_pat] = Ppx_hash_lib.Std.Hash.create () in
          [%e hsv_expr])
    ]

let hash_structure_item_of_td td =
  let loc = td.ptype_loc in
  match td.ptype_params with
  | _::_ -> []
  | [] -> [
      let bnd = pvar ~loc (hash_ td.ptype_name.txt) in
      let typ = combinator_type_of_type_declaration td ~f:hash_type in
      let pat = ppat_constraint ~loc bnd typ in
      let expected_scope, expr =
        let is_simple_type ty = match recognize_simple_type ty with
          | Some _ -> true
          | None -> false
        in
        match td.ptype_kind, td.ptype_manifest with
        | Ptype_abstract, Some ty when is_simple_type ty ->
          `uses_rhs,
          (hash_of_ty_fun ~special_case_simple_types:true ~type_constraint:false ty)
        | _ ->
          `uses_hash_fold_t_being_defined,
          (hash_of_ty_fun ~special_case_simple_types:false ~type_constraint:false
             { ptyp_loc = loc;
               ptyp_loc_stack = [];
               ptyp_attributes = [];
               ptyp_desc =
                 Ptyp_constr ({ loc; txt = Lident td.ptype_name.txt }, []); })
      in
      expected_scope, value_binding ~loc ~pat ~expr:(eta_expand ~loc expr)
    ]

let hash_fold_structure_item_of_td td ~rec_flag =
  let loc = td.ptype_loc in
  let arg = "arg" in
  let body =
    let v      = evar ~loc arg in
    match td.ptype_kind with
    | Ptype_variant cds -> hash_sum ~loc cds v
    | Ptype_record  lds -> hash_fold_of_record ~loc lds v
    | Ptype_open ->
      Location.raise_errorf ~loc
        "ppx_hash: open types are not supported"
    | Ptype_abstract ->
      match td.ptype_manifest with
      | None -> hash_fold_of_abstract ~loc td.ptype_name.txt v
      | Some ty ->
        match ty.ptyp_desc with
        | Ptyp_variant (_, Open, _) | Ptyp_variant (_, Closed, Some (_ :: _)) ->
          Location.raise_errorf ~loc:ty.ptyp_loc
            "ppx_hash: cannot hash open polymorphic variant types"
        | Ptyp_variant (row_fields, _, _) ->
          hash_variant ~loc row_fields v
        | _ ->
          hash_fold_of_ty ty v
  in
  let vars = List.map td.ptype_params ~f:(fun p -> (get_type_param_name p)) in
  let extra_names = List.map vars ~f:(fun x -> tp_name x.txt) in
  let hsv_pat, hsv_expr = Hsv_expr.to_expression ~loc body in
  let patts = List.map extra_names ~f:(pvar ~loc) @ [ hsv_pat; pvar ~loc arg ] in
  let bnd = pvar ~loc (hash_fold_ td.ptype_name.txt) in
  let scheme = combinator_type_of_type_declaration td ~f:hash_fold_type in
  let pat = ppat_constraint ~loc bnd (ptyp_poly ~loc vars scheme) in
  let expr =
    eta_reduce_if_possible_and_nonrec ~rec_flag (
      eabstract ~loc patts hsv_expr)
  in
  let use_rigid_variables = match td.ptype_kind with | Ptype_variant _ -> true | _ -> false in
  let expr =
    if use_rigid_variables then
      let type_name = td.ptype_name.txt in
      List.fold_right vars ~f:(fun s ->
        pexp_newtype ~loc { txt = rigid_type_var ~type_name s.txt; loc = s.loc; })
        ~init:(pexp_constraint ~loc expr (make_type_rigid ~type_name scheme))
    else
      expr
  in
  value_binding ~loc ~pat ~expr

let pstr_value ~loc rec_flag bindings = match bindings with
  | [] -> []
  | nonempty_bindings ->
    (* [pstr_value] with zero bindings is invalid *)
    [ pstr_value ~loc rec_flag nonempty_bindings ]

let str_type_decl ~loc ~path:_ (rec_flag, tds) =
  let tds = List.map tds ~f:name_type_params_in_td in
  let rec_flag =
    (object
      inherit type_is_recursive rec_flag tds as super

      method! label_declaration ld =
        match fst (should_ignore_label_declaration ld) with
        | `ignore -> ()
        | `incorporate -> super#label_declaration ld

      method! core_type ty =
        if core_type_is_ignored ty then ()
        else super#core_type ty

    end)#go ()
  in
  let hash_fold_bindings = List.map ~f:(hash_fold_structure_item_of_td ~rec_flag) tds in
  let hash_bindings =
    List.concat (List.map ~f:hash_structure_item_of_td tds)
  in
  match rec_flag with
  | Recursive ->
    (* if we wanted to maximize the scope hygiene here this would be, in this order:
       - recursive group of [hash_fold]
       - nonrecursive group of [hash] that are [`uses_hash_fold_t_being_defined]
       - recursive group of [hash] that are [`uses_rhs]
         but fighting the "unused rec flag" warning is just way too hard *)
    pstr_value ~loc Recursive (hash_fold_bindings @ List.map ~f:snd hash_bindings)
  | Nonrecursive ->
    let rely_on_hash_fold_t, use_rhs =
      List.partition_map hash_bindings ~f:(function
        | `uses_hash_fold_t_being_defined, binding ->
          `Fst binding
        | `uses_rhs, binding ->
          `Snd binding)
    in
    pstr_value ~loc Nonrecursive (hash_fold_bindings @ use_rhs) @
    pstr_value ~loc Nonrecursive rely_on_hash_fold_t

let sig_type_decl ~loc:_ ~path:_ (_rec_flag, tds) =
  List.concat (List.map tds ~f:(fun td ->
    let monomorphic = List.is_empty td.ptype_params in
    let definition ~f_type ~f_name =
      let type_ =
        combinator_type_of_type_declaration td ~f:f_type
      in
      let name =
        let tn = td.ptype_name.txt in
        f_name tn
      in
      let loc = td.ptype_loc in
      psig_value ~loc (
        value_description ~loc ~name:{ td.ptype_name with txt = name }
          ~type_ ~prim:[])
    in
    List.concat [
      [                     definition ~f_type:hash_fold_type ~f_name:hash_fold_];
      (if monomorphic then [definition ~f_type:hash_type      ~f_name:hash_     ] else []);
    ]))

let hash_fold_core_type ty = hash_fold_of_ty_fun ~type_constraint:true ty
let hash_core_type ty = hash_of_ty_fun ~special_case_simple_types:true ~type_constraint:true ty
