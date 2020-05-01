open Base
open Ppxlib
open Ast_builder.Default

(* The scope in which to find [Optional_syntax]. [From_module] means using
   module.Optional_syntax.Optional_syntax *)
type module_scope = 
  | Use_optional_syntax
  | Use_optional_syntax_optional_syntax
  | From_module of longident loc

module Matched_expression_element = struct
  type t =
    { module_ : module_scope
    ; exp : expression
    }
end

type t =
  { default_module : module_scope
  ; original_matched_expr : expression
  ; elements : Matched_expression_element.t list
  ; match_loc : Location.t
  ; cases : case list
  }

let module_scope_of_option = function
  | None -> Use_optional_syntax
  | Some module_ -> From_module module_

let infer_module_from_core_type ~module_ (core_type : core_type) =
  let default = module_scope_of_option module_ in
  match core_type.ptyp_desc with
  | Ptyp_constr (longident, _params) ->
    begin match longident.txt with
    | Lident _ -> Use_optional_syntax_optional_syntax
    | Ldot (longident, _label) ->
      From_module { txt = longident; loc = core_type.ptyp_loc}
    | Lapply _ -> default
    end
  | _ -> default

let expand_matched_expr ~(module_ : longident loc option) matched_expr =
  let individual_exprs =
    match matched_expr.pexp_desc with
    | Pexp_tuple exprs -> exprs
    | _ -> [ matched_expr ]
  in
  List.map individual_exprs
    ~f:(fun exp ->
      match exp.pexp_desc with
      | Pexp_constraint (_exp, core_type) ->
        { Matched_expression_element.
          module_ = infer_module_from_core_type ~module_ core_type
        ; exp
        }
      | _ ->
        { module_ = module_scope_of_option module_
        ; exp
        }
    )

let optional_syntax_str = "Optional_syntax"

let optional_syntax ~module_ : Longident.t =
  match (module_ : module_scope) with
  | Use_optional_syntax ->
    Lident optional_syntax_str
  | Use_optional_syntax_optional_syntax ->
    Ldot (Lident optional_syntax_str, optional_syntax_str)
  | From_module id  ->
    Ldot (Ldot (id.txt, optional_syntax_str), optional_syntax_str)
;;

let eoperator ~loc ~module_ func =
  let lid : Longident.t = Ldot (optional_syntax ~module_, func) in
  pexp_ident ~loc (Located.mk ~loc lid)

let eunsafe_value = eoperator "unsafe_value"
let eis_none = eoperator "is_none"

let assert_no_guard = function
  | None -> ()
  | Some guard ->
    Location.raise_errorf ~loc:guard.pexp_loc "guards are not supported in [%%optional ]"

let rec assert_binder pat =
  match pat.ppat_desc with
  | Ppat_constraint (pat, _) ->
    (* Allow "Some (_ : typ)" *)
    assert_binder pat
  | Ppat_var _
  | Ppat_any ->
    ()
  | _ ->
    Location.raise_errorf ~loc:pat.ppat_loc
      "sub patterns are restricted to variable names and wildcards"

let disable_exhaustivity_warning e =
  let attr =
    let loc = Location.none in
    attribute
      ~loc
      ~name:({ Location. loc; txt = "ocaml.warning" })
      ~payload:(PStr [ pstr_eval ~loc (estring ~loc "-8") [] ])
  in
  { e with pexp_attributes = attr :: e.pexp_attributes }

let varname i = Printf.sprintf "__ppx_optional_e_%i" i
let evar ~loc i = evar ~loc (varname i)
let pvar ~loc i = pvar ~loc (varname i)

let get_pattern_and_binding ~module_ i pattern =
  let loc = pattern.ppat_loc in
  let pat, binding_opt =
    match pattern with
    | [%pat? Some [%p? x]] ->
      assert_binder x;
      let binding =
        value_binding ~loc ~pat:[%pat? ([%p x] : _)]
          ~expr:(eapply ~loc (eunsafe_value ~loc ~module_) [evar ~loc i])
      in
      [%pat? false], Some binding
    | [%pat? None] -> [%pat? true], None
    | [%pat? _] -> pattern, None
    | _ ->
      Location.raise_errorf ~loc:pattern.ppat_loc
        "only None, Some and _ are supported in [%%optional ]"
  in
  (* by only using the ppat_desc from the pattern we just generated we ensure that the
     location of the original pattern is kept. *)
  { pattern with ppat_desc = pat.ppat_desc }, binding_opt

let rewrite_case ~match_loc ~modules ~default_module
      { pc_lhs = pat; pc_rhs = body; pc_guard }
  =
  assert_no_guard pc_guard;
  let modules_array = Array.of_list modules in
  let get_module i =
    (* Sadly, we need to be able to handle the case when the length of the matched
       expression doesn't equal the length of the case, in order to produce useful 
       error messages (with the proper types). *)
    if i < Array.length modules_array
    then modules_array.(i)
    else default_module
  in
  let ppat_desc, bindings =
    match pat.ppat_desc with
    | Ppat_tuple patts ->
      let patts, binding_opts =
        List.mapi patts
          ~f:(fun i patt ->
            let module_ = get_module i in
            get_pattern_and_binding ~module_ i patt
          )
        |> List.unzip
      in
      Ppat_tuple patts, List.filter_map binding_opts ~f:Fn.id
    | _ ->
      let pat, binding_opt =
        get_pattern_and_binding 0 pat ~module_:(List.hd_exn modules)
      in
      pat.ppat_desc, Option.to_list binding_opt
  in
  let pc_lhs = { pat with ppat_desc } in
  let pc_rhs =
    match bindings with
    | [] -> body
    | _  -> pexp_let ~loc:match_loc Nonrecursive bindings body
  in
  { pc_lhs; pc_rhs; pc_guard }

(** Take the matched expression and replace all its components by a variable, which will
    have been bound previously, wrapped by [wrapper].
    We do keep the location of the initial component for the new one. *)
let rewrite_matched_expr t ~wrapper =
  let subst_and_wrap i { Matched_expression_element. module_; exp } =
    let loc = exp.pexp_loc in
    { (wrapper ~module_ (evar ~loc i)) with pexp_loc = loc }
  in
  let pexp_desc =
    match t.elements with
    | [ singleton ] ->
        (subst_and_wrap 0 singleton).pexp_desc
    | list ->
      Pexp_tuple (List.mapi list ~f:subst_and_wrap)
  in
  { t.original_matched_expr with pexp_desc }

let real_match t =
  let new_matched_expr =
    rewrite_matched_expr t ~wrapper:(fun ~module_ expr ->
      let loc = expr.pexp_loc in
      eapply ~loc (eis_none ~loc ~module_) [expr]
    )
  in
  let modules = List.map t.elements ~f:(fun { module_ ; _} -> module_) in
  let cases =
    List.map t.cases 
      ~f:(rewrite_case ~match_loc:t.match_loc ~modules ~default_module:t.default_module)
  in
  (* we can disable the warning here as we rely on the other match we generate for
     error messages. *)
  disable_exhaustivity_warning (pexp_match ~loc:t.match_loc new_matched_expr cases)

let fake_match t =
  let new_matched_expr =
    rewrite_matched_expr t ~wrapper:(fun ~module_ expr ->
      let loc = expr.pexp_loc in
      [%expr
         (* This code will never be executed, it is just here so the type checker
            generates nice error messages. *)
        if [%e eis_none ~loc ~module_] [%e expr] then None
        else Some ([%e eunsafe_value ~loc ~module_] [%e expr])
      ]
    )
  in
  pexp_match ~loc:t.match_loc new_matched_expr t.cases

let bindings_for_matched_expr matched_expr =
  let bind i expr =
    let loc = expr.pexp_loc in
    value_binding ~loc ~pat:(pvar ~loc i) ~expr
  in
  List.mapi matched_expr ~f:(fun i { Matched_expression_element. exp; _ } -> bind i exp)

let expand_match ~match_loc ~(module_ : longident loc option) matched_expr cases =
  let t =
    { default_module = module_scope_of_option module_
    ; original_matched_expr = matched_expr
    ; elements = expand_matched_expr ~module_ matched_expr
    ; match_loc
    ; cases
    }
  in
  let fake_match =
    (* The types in this branch actually match what the user would expect given the source
       code, so we tell merlin to do all its work in here. *)
    Merlin_helpers.focus_expression (fake_match t)
  in
  let real_match =
    (* The types here actually have nothing to do with what's in the source ([bool]
       appears for example), so we tell merlin to avoid that branch. *)
    Merlin_helpers.hide_expression (real_match t)
  in
  let bindings = bindings_for_matched_expr t.elements in
  let loc = match_loc in
  pexp_let ~loc Nonrecursive bindings
    (pexp_ifthenelse ~loc (ebool ~loc false) fake_match (Some real_match))


(* We add the indirection instead of directly matching on [pexp_match] when declaring the
   extension because we want more informative error messages than "Extension was not
   translated". *)
let expand_match ~loc ~path:_ ~arg:(module_ : longident loc option) e =
  Ast_pattern.parse Ast_pattern.(pexp_match __ __) loc e ~on_error:(fun () ->
    Location.raise_errorf ~loc "[%%optional ] must apply to a match statement"
  ) (expand_match ~match_loc:e.pexp_loc ~module_)

let optional =
  Extension.declare_with_path_arg "optional" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_match

let () =
  Driver.register_transformation "optional"
    ~extensions:[ optional ]
