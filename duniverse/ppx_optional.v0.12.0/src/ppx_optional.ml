open Base
open Ppxlib
open Ast_builder.Default

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
    { Location. loc; txt = "ocaml.warning" },
    PStr [ pstr_eval ~loc (estring ~loc "-8") [] ]
  in
  { e with pexp_attributes = attr :: e.pexp_attributes }

let varname i = Printf.sprintf "__ppx_optional_e_%i" i
let evar ~loc i = evar ~loc (varname i)
let pvar ~loc i = pvar ~loc (varname i)

let get_pattern_and_binding i pattern =
  let loc = pattern.ppat_loc in
  let pat, binding_opt =
    match pattern with
    | [%pat? Some [%p? x]] ->
      assert_binder x;
      let binding =
        value_binding ~loc ~pat:x
          ~expr:(eapply ~loc [%expr Optional_syntax.unsafe_value] [evar ~loc i])
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


let rewrite_case ~match_loc { pc_lhs = pat; pc_rhs = body; pc_guard } =
  assert_no_guard pc_guard;
  let ppat_desc, bindings =
    match pat.ppat_desc with
    | Ppat_tuple patts ->
      let patts, binding_opts = List.unzip (List.mapi patts ~f:get_pattern_and_binding) in
      Ppat_tuple patts, List.filter_map binding_opts ~f:Fn.id
    | _ ->
      let pat, binding_opt = get_pattern_and_binding 0 pat in
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
let rewrite_matched_expr ~wrapper expr =
  let subst_and_wrap i expr =
    let loc = expr.pexp_loc in
    { (wrapper (evar ~loc i)) with pexp_loc = loc }
  in
  let pexp_desc =
    match expr.pexp_desc with
    | Pexp_tuple exprs -> Pexp_tuple (List.mapi exprs ~f:subst_and_wrap)
    | _ -> (subst_and_wrap 0 expr).pexp_desc
  in
  { expr with pexp_desc }

let real_match ~match_loc matched_expr cases =
  let matched_expr =
    rewrite_matched_expr matched_expr ~wrapper:(fun expr ->
      let loc = expr.pexp_loc in
      eapply ~loc [%expr Optional_syntax.is_none] [expr]
    )
  in
  let cases = List.map cases ~f:(rewrite_case ~match_loc) in
  (* we can disable the warning here as we rely on the other match we generate for
     error messages. *)
  disable_exhaustivity_warning (pexp_match ~loc:match_loc matched_expr cases)

let fake_match ~match_loc matched_expr cases =
  let matched_expr =
    rewrite_matched_expr matched_expr ~wrapper:(fun expr ->
      let loc = expr.pexp_loc in
      [%expr
         (* This code will never be executed, it is just here so the type checker
            generates nice error messages. *)
        if Optional_syntax.is_none [%e expr] then None
        else Some (Optional_syntax.unsafe_value [%e expr])
      ]
    )
  in
  pexp_match ~loc:match_loc matched_expr cases

let bindings_for_matched_expr matched_expr =
  let bind i expr =
    let loc = expr.pexp_loc in
    value_binding ~loc ~pat:(pvar ~loc i) ~expr
  in
  match matched_expr.pexp_desc with
  | Pexp_tuple exprs -> List.mapi exprs ~f:bind
  | _ -> [bind 0 matched_expr]

let expand_match ~match_loc matched_expr cases =
  let fake_match =
    (* The types in this branch actually match what the user would expect given the source
       code, so we tell merlin to do all its work in here. *)
    Merlin_helpers.focus_expression (fake_match ~match_loc matched_expr cases)
  in
  let real_match =
    (* The types here actually have nothing to do with what's in the source ([bool]
       appears for example), so we tell merlin to avoid that branch. *)
    Merlin_helpers.hide_expression (real_match ~match_loc matched_expr cases)
  in
  let bindings = bindings_for_matched_expr matched_expr in
  let loc = match_loc in
  pexp_let ~loc Nonrecursive bindings
    (pexp_ifthenelse ~loc (ebool ~loc false) fake_match (Some real_match))

(* We add the indirection instead of directly matching on [pexp_match] when declaring the
   extension because we want more informative error messages than "Extension was not
   translated". *)
let expand_match ~loc ~path:_ e =
  Ast_pattern.parse Ast_pattern.(pexp_match __ __) loc e ~on_error:(fun () ->
    Location.raise_errorf ~loc "[%%optional ] must apply to a match statement"
  ) (expand_match ~match_loc:e.pexp_loc)

let optional =
  Extension.declare "optional" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_match

let () =
  Driver.register_transformation "optional"
    ~extensions:[ optional ]
