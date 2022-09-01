open! Base
open! Ppxlib
open Ast_builder.Default
open Helpers

module Reference = struct
  type t =
    { binds : value_binding list list
    ; ident : longident_loc
    ; args : (arg_label * expression) list
    }

  let bind t binds = { t with binds = binds :: t.binds }

  let maybe_apply { binds; ident; args } ~loc maybe_arg =
    let ident = pexp_ident ~loc ident in
    let args =
      match maybe_arg with
      | None -> args
      | Some arg -> args @ [ Nolabel, arg ]
    in
    let expr =
      match args with
      | [] -> ident
      | _ -> pexp_apply ~loc ident args
    in
    with_let ~loc ~binds expr
  ;;

  let apply t ~loc arg = maybe_apply t ~loc (Some arg)
  let to_expression t ~loc = maybe_apply t ~loc None

  let to_value_expression t ~loc =
    match t with
    | { binds = []; ident; args = [] } -> pexp_ident ~loc ident
    | _ -> fresh_lambda ~loc (fun ~arg -> apply t ~loc arg)
  ;;
end

module Lambda = struct
  type t =
    { binds : value_binding list list
    ; cases : cases
    }

  let bind t binds = { t with binds = binds :: t.binds }

  (* generic case: use [function] or [match] *)
  let maybe_apply_generic ~loc ~binds maybe_arg cases =
    let expr =
      match maybe_arg with
      | None -> pexp_function ~loc cases
      | Some arg -> pexp_match ~loc arg cases
    in
    with_let ~loc ~binds expr
  ;;

  (* zero cases: synthesize an "impossible" case, i.e. [| _ -> .] *)
  let maybe_apply_impossible ~loc ~binds maybe_arg =
    maybe_apply_generic
      ~loc
      ~binds
      maybe_arg
      [ case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:(pexp_unreachable ~loc) ]
  ;;

  (* one case without guard: use [fun] or [let] *)
  let maybe_apply_simple ~loc ~binds maybe_arg pat body =
    let expr =
      match maybe_arg with
      | None -> pexp_fun ~loc Nolabel None pat body
      | Some arg -> pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat ~expr:arg ] body
    in
    with_let ~loc ~binds expr
  ;;

  (* shared special-casing logic for [apply] and [to_expression] *)
  let maybe_apply t ~loc maybe_arg =
    match t with
    | { binds; cases = [] } -> maybe_apply_impossible ~loc ~binds maybe_arg
    | { binds; cases = [ { pc_lhs; pc_guard = None; pc_rhs } ] } ->
      maybe_apply_simple ~loc ~binds maybe_arg pc_lhs pc_rhs
    | { binds; cases } -> maybe_apply_generic ~loc ~binds maybe_arg cases
  ;;

  let apply t ~loc arg = maybe_apply t ~loc (Some arg)
  let to_expression t ~loc = maybe_apply t ~loc None

  let to_value_expression t ~loc =
    match t with
    | { binds = []; cases = _ } ->
      (* lambdas without [let] are already values *)
      let expr = to_expression t ~loc in
      assert (is_value_expression expr);
      expr
    | _ -> fresh_lambda ~loc (fun ~arg -> apply t ~loc arg)
  ;;
end

type t =
  | Reference of Reference.t
  | Lambda of Lambda.t

let of_lambda cases = Lambda { binds = []; cases }

let of_reference_exn expr =
  match expr.pexp_desc with
  | Pexp_ident ident -> Reference { binds = []; ident; args = [] }
  | Pexp_apply ({ pexp_desc = Pexp_ident ident; _ }, args) ->
    Reference { binds = []; ident; args }
  | _ ->
    Location.raise_errorf
      ~loc:expr.pexp_loc
      "ppx_sexp_conv: internal error.\n\
       [Conversion.of_reference_exn] expected an identifier possibly applied to arguments.\n\
       Instead, got:\n\
       %s"
      (Pprintast.string_of_expression expr)
;;

let to_expression t ~loc =
  match t with
  | Reference reference -> Reference.to_expression ~loc reference
  | Lambda lambda -> Lambda.to_expression ~loc lambda
;;

let to_value_expression t ~loc =
  match t with
  | Reference reference -> Reference.to_value_expression ~loc reference
  | Lambda lambda -> Lambda.to_value_expression ~loc lambda
;;

let apply t ~loc e =
  match t with
  | Reference reference -> Reference.apply ~loc reference e
  | Lambda lambda -> Lambda.apply ~loc lambda e
;;

let bind t binds =
  match t with
  | Reference reference -> Reference (Reference.bind reference binds)
  | Lambda lambda -> Lambda (Lambda.bind lambda binds)
;;

module Apply_all = struct
  type t =
    { bindings : value_binding list
    ; arguments : pattern list
    ; converted : expression list
    }
end

let gen_symbols list ~prefix =
  List.mapi list ~f:(fun i _ -> gen_symbol ~prefix:(prefix ^ Int.to_string i) ())
;;

let apply_all ts ~loc =
  let arguments_names = gen_symbols ts ~prefix:"arg" in
  let converted_names = gen_symbols ts ~prefix:"res" in
  let bindings =
    List.map3_exn ts arguments_names converted_names ~f:(fun t arg conv ->
      let expr = apply ~loc t (evar ~loc arg) in
      value_binding ~loc ~pat:(pvar ~loc conv) ~expr)
  in
  ({ bindings
   ; arguments = List.map arguments_names ~f:(pvar ~loc)
   ; converted = List.map converted_names ~f:(evar ~loc)
   }
   : Apply_all.t)
;;
