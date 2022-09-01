open! Base
open! Ppxlib
open Ast_builder.Default

let ( --> ) lhs rhs = case ~guard:None ~lhs ~rhs

(* Utility functions *)

let replace_variables_by_underscores =
  let map =
    object
      inherit Ast_traverse.map as super

      method! core_type_desc =
        function
        | Ptyp_var _ -> Ptyp_any
        | t -> super#core_type_desc t
    end
  in
  map#core_type
;;

let make_rigid_types tps =
  List.fold
    tps
    ~init:(Map.empty (module String))
    ~f:(fun map tp ->
      Map.update map tp.txt ~f:(function
        | None -> Fresh_name.of_string_loc tp
        | Some fresh ->
          (* Ignore duplicate names, the typechecker will raise after expansion. *)
          fresh))
;;

let find_rigid_type ~loc ~rigid_types name =
  match Map.find rigid_types name with
  | Some tp -> Fresh_name.to_string_loc tp
  | None ->
    (* Ignore unbound type names, the typechecker will raise after expansion. *)
    { txt = name; loc }
;;

let make_type_rigid ~rigid_types =
  let map =
    object
      inherit Ast_traverse.map as super

      method! core_type ty =
        let ptyp_desc =
          match ty.ptyp_desc with
          | Ptyp_var s ->
            Ptyp_constr
              (Located.map_lident (find_rigid_type ~loc:ty.ptyp_loc ~rigid_types s), [])
          | desc -> super#core_type_desc desc
        in
        { ty with ptyp_desc }
    end
  in
  map#core_type
;;

(* Generates the quantified type [ ! 'a .. 'z . (make_mono_type t ('a .. 'z)) ] or
   [type a .. z. make_mono_type t (a .. z)] when [use_rigid_variables] is true.
   Annotation are needed for non regular recursive datatypes and gadt when the return type
   of constructors are constrained. Unfortunately, putting rigid variables everywhere does
   not work because of certains types with constraints. We thus only use rigid variables
   for sum types, which includes all GADTs. *)

let tvars_of_core_type : core_type -> string list =
  let tvars =
    object
      inherit [string list] Ast_traverse.fold as super

      method! core_type x acc =
        match x.ptyp_desc with
        | Ptyp_var x -> if List.mem acc x ~equal:String.equal then acc else x :: acc
        | _ -> super#core_type x acc
    end
  in
  fun typ -> List.rev (tvars#core_type typ [])
;;

let constrained_function_binding
      (* placing a suitably polymorphic or rigid type constraint on the pattern or body *)
      (loc : Location.t)
      (td : type_declaration)
      (typ : core_type)
      ~(tps : string loc list)
      ~(func_name : string)
      (body : expression)
  =
  let vars = tvars_of_core_type typ in
  let has_vars =
    match vars with
    | [] -> false
    | _ :: _ -> true
  in
  let pat =
    let pat = pvar ~loc func_name in
    if not has_vars
    then pat
    else (
      let vars = List.map ~f:(fun txt -> { txt; loc }) vars in
      ppat_constraint ~loc pat (ptyp_poly ~loc vars typ))
  in
  let body =
    let use_rigid_variables =
      match td.ptype_kind with
      | Ptype_variant _ -> true
      | _ -> false
    in
    if use_rigid_variables
    then (
      let rigid_types = make_rigid_types tps in
      List.fold_right
        tps
        ~f:(fun tp body ->
          pexp_newtype ~loc (find_rigid_type ~loc:tp.loc ~rigid_types tp.txt) body)
        ~init:(pexp_constraint ~loc body (make_type_rigid ~rigid_types typ)))
    else if has_vars
    then body
    else pexp_constraint ~loc body typ
  in
  value_binding ~loc ~pat ~expr:body
;;

let with_let ~loc ~binds body =
  List.fold_right binds ~init:body ~f:(pexp_let ~loc Nonrecursive)
;;

let fresh_lambda ~loc apply =
  let var = gen_symbol ~prefix:"x" () in
  let pat = pvar ~loc var in
  let arg = evar ~loc var in
  let body = apply ~arg in
  pexp_fun ~loc Nolabel None pat body
;;

let rec is_value_expression expr =
  match expr.pexp_desc with
  (* Syntactic values. *)
  | Pexp_ident _ | Pexp_constant _ | Pexp_function _ | Pexp_fun _ | Pexp_lazy _ -> true
  (* Type-only wrappers; we check their contents. *)
  | Pexp_constraint (expr, (_ : core_type))
  | Pexp_coerce (expr, (_ : core_type option), (_ : core_type))
  | Pexp_newtype ((_ : string loc), expr) -> is_value_expression expr
  (* Allocating constructors; they are only values if all of their contents are. *)
  | Pexp_tuple exprs -> List.for_all exprs ~f:is_value_expression
  | Pexp_construct (_, maybe_expr) -> Option.for_all maybe_expr ~f:is_value_expression
  | Pexp_variant (_, maybe_expr) -> Option.for_all maybe_expr ~f:is_value_expression
  | Pexp_record (fields, maybe_expr) ->
    List.for_all fields ~f:(fun (_, expr) -> is_value_expression expr)
    && Option.for_all maybe_expr ~f:is_value_expression
  (* Not values, or not always values. We make a conservative approximation. *)
  | Pexp_unreachable
  | Pexp_let _
  | Pexp_apply _
  | Pexp_match _
  | Pexp_try _
  | Pexp_field _
  | Pexp_setfield _
  | Pexp_array _
  | Pexp_ifthenelse _
  | Pexp_sequence _
  | Pexp_while _
  | Pexp_for _
  | Pexp_send _
  | Pexp_new _
  | Pexp_setinstvar _
  | Pexp_override _
  | Pexp_letmodule _
  | Pexp_letexception _
  | Pexp_assert _
  | Pexp_poly _
  | Pexp_object _
  | Pexp_pack _
  | Pexp_open _
  | Pexp_letop _
  | Pexp_extension _ -> false
;;

let really_recursive_respecting_opaque rec_flag tds =
  (object
    inherit type_is_recursive rec_flag tds as super

    method! core_type ctype =
      match ctype with
      | _ when Option.is_some (Attribute.get ~mark_as_seen:false Attrs.opaque ctype) ->
        ()
      | [%type: [%t? _] sexp_opaque] -> ()
      | _ -> super#core_type ctype
  end)
  #go
    ()
;;
