#include "../compat_macros.cppo"

open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "eq"
let raise_errorf = Ppx_deriving.raise_errorf

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

let attr_nobuiltin attrs =
  Ppx_deriving.(attrs |> attr ~deriver "nobuiltin" |> Arg.get_flag ~deriver)

let attr_equal attrs =
  Ppx_deriving.(attrs |> attr ~deriver "equal" |> Arg.(get_attr ~deriver expr))

let argn kind =
  Printf.sprintf (match kind with `lhs -> "lhs%d" | `rhs -> "rhs%d")

let argl kind =
  Printf.sprintf (match kind with `lhs -> "lhs%s" | `rhs -> "rhs%s")

let pattn side typs =
  List.mapi (fun i _ -> pvar (argn side i)) typs

let pattl side labels =
  List.map (fun { pld_name = { txt = n } } -> n, pvar (argl side n)) labels

let pconstrrec name fields =
  pconstr name [precord ~closed:Closed fields]

let core_type_of_decl ~options ~path type_decl =
  parse_options options;
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: [%t var] -> [%t var] -> Ppx_deriving_runtime.bool])
    type_decl
    [%type: [%t typ] -> [%t typ] -> Ppx_deriving_runtime.bool]

let sig_of_type ~options ~path type_decl =
  parse_options options;
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "equal") type_decl))
             (core_type_of_decl ~options ~path type_decl))]

let rec exprn quoter typs =
  typs |> List.mapi (fun i typ ->
    app (expr_of_typ quoter typ) [evar (argn `lhs i); evar (argn `rhs i)])

and exprl quoter typs =
  typs |> List.map (fun ({ pld_name = { txt = n }; pld_loc; _ } as pld) ->
    with_default_loc pld_loc @@ fun () ->
    app (expr_of_label_decl quoter pld)
      [evar (argl `lhs n); evar (argl `rhs n)])

and expr_of_label_decl quoter { pld_type; pld_attributes } =
  let attrs = pld_type.ptyp_attributes @ pld_attributes in
  expr_of_typ quoter { pld_type with ptyp_attributes = attrs }

and expr_of_typ quoter typ =
  let typ = Ppx_deriving.remove_pervasives ~deriver typ in
  let expr_of_typ = expr_of_typ quoter in
  match attr_equal typ.ptyp_attributes with
  | Some fn -> Ppx_deriving.quote quoter fn
  | None ->
    match typ with
    | [%type: _] -> [%expr fun _ _ -> true]
    | { ptyp_desc = Ptyp_constr _ } ->
      let builtin = not (attr_nobuiltin typ.ptyp_attributes) in
      begin match builtin, typ with
      | true, [%type: unit] ->
        [%expr fun (_:unit) (_:unit) -> true]
      | true, ([%type: int] | [%type: int32] | [%type: Int32.t] |
               [%type: int64] | [%type: Int64.t] | [%type: nativeint] |
               [%type: Nativeint.t] | [%type: float] | [%type: bool] |
               [%type: char] | [%type: string] | [%type: bytes]) ->
        [%expr (fun (a:[%t typ]) b -> a = b)]
      | true, [%type: [%t? typ] ref] ->
        [%expr fun a b -> [%e expr_of_typ typ] !a !b]
      | true, [%type: [%t? typ] list] ->
        [%expr
          let rec loop x y =
            match x, y with
            | [], [] -> true
            | a :: x, b :: y -> [%e expr_of_typ typ] a b && loop x y
            | _ -> false
          in (fun x y -> loop x y)]
      | true, [%type: [%t? typ] array] ->
        [%expr fun x y ->
          let rec loop i =
            i = Array.length x || ([%e expr_of_typ typ] x.(i) y.(i) && loop (i + 1))
          in Array.length x = Array.length y && loop 0]
      | true, [%type: [%t? typ] option] ->
        [%expr fun x y ->
          match x, y with
          | None, None -> true
          | Some a, Some b -> [%e expr_of_typ typ] a b
          | _ -> false]
      | true, ([%type: ([%t? ok_t], [%t? err_t]) result] |
               [%type: ([%t? ok_t], [%t? err_t]) Result.result]) ->
        [%expr fun x y ->
          match x, y with
          | Result.Ok a, Result.Ok b -> [%e expr_of_typ ok_t] a b
          | Result.Error a, Result.Error b -> [%e expr_of_typ err_t] a b
          | _ -> false]
      | true, ([%type: [%t? typ] lazy_t] | [%type: [%t? typ] Lazy.t]) ->
        [%expr fun (lazy x) (lazy y) -> [%e expr_of_typ typ] x y]
      | _, { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
        let equal_fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "equal") lid)) in
        let fwd = app (Ppx_deriving.quote quoter equal_fn) (List.map expr_of_typ args) in
        (* eta-expansion is necessary for recursive groups *)
        [%expr fun x -> [%e fwd] x]
      | _ -> assert false
      end
    | { ptyp_desc = Ptyp_tuple typs } ->
      [%expr fun [%p ptuple (pattn `lhs typs)] [%p ptuple (pattn `rhs typs)] ->
        [%e exprn quoter typs |> Ppx_deriving.(fold_exprs (binop_reduce [%expr (&&)]))]]
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let cases =
        (fields |> List.map (fun field ->
          let pdup f = ptuple [f "lhs"; f "rhs"] in
          let variant label popt =
#if OCAML_VERSION < (4, 06, 0)
            Pat.variant label popt
#else
            Pat.variant label.txt popt
#endif
          in
          match field with
          | Rtag_patt(label, true (*empty*), []) ->
            Exp.case (pdup (fun _ -> variant label None)) [%expr true]
          | Rtag_patt(label, false, [typ]) ->
            Exp.case (pdup (fun var -> variant label (Some (pvar var))))
                     (app (expr_of_typ typ) [evar "lhs"; evar "rhs"])
          | Rinherit_patt({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
            Exp.case (pdup (fun var -> Pat.alias (Pat.type_ tname) (mknoloc var)))
                     (app (expr_of_typ typ) [evar "lhs"; evar "rhs"])
          | _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                         deriver (Ppx_deriving.string_of_core_type typ))) @
        [Exp.case (pvar "_") [%expr false]]
      in
      [%expr fun lhs rhs -> [%e Exp.match_ [%expr lhs, rhs] cases]]
    | { ptyp_desc = Ptyp_var name } -> evar ("poly_"^name)
    | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
    | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ)

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  parse_options options;
  let quoter = Ppx_deriving.create_quoter () in
  let comparator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> expr_of_typ quoter manifest
    | Ptype_variant constrs, _ ->
      let cases =
        (constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args; pcd_loc } ->
          with_default_loc pcd_loc @@ fun () ->
          match pcd_args with
          | Pcstr_tuple(typs) ->
            exprn quoter typs |>
            Ppx_deriving.(fold_exprs ~unit:[%expr true] (binop_reduce [%expr (&&)])) |>
            Exp.case (ptuple [pconstr name (pattn `lhs typs);
                              pconstr name (pattn `rhs typs)])
#if OCAML_VERSION >= (4, 03, 0)
          | Pcstr_record(labels) ->
            exprl quoter labels |>
            Ppx_deriving.(fold_exprs ~unit:[%expr true] (binop_reduce [%expr (&&)])) |>
            Exp.case (ptuple [pconstrrec name (pattl `lhs labels);
                              pconstrrec name (pattl `rhs labels)])
#endif
          )) @
        [Exp.case (pvar "_") [%expr false]]
      in
      [%expr fun lhs rhs -> [%e Exp.match_ [%expr lhs, rhs] cases]]
    | Ptype_record labels, _ ->
      let exprs =
        labels |> List.map (fun ({ pld_loc; pld_name = { txt = name }; _ } as pld) ->
          with_default_loc pld_loc @@ fun () ->
          (* combine attributes of type and label *)
          let field obj = Exp.field obj (mknoloc (Lident name)) in
          app (expr_of_label_decl quoter pld)
            [field (evar "lhs"); field (evar "rhs")])
      in
      [%expr fun lhs rhs -> [%e exprs |> Ppx_deriving.(fold_exprs (binop_reduce [%expr (&&)]))]]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _ ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let out_type =
    Ppx_deriving.strong_type_of_type @@
      core_type_of_decl  ~options ~path type_decl in
  let eq_var =
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "equal") type_decl) in
  [Vb.mk ~attrs:[Ppx_deriving.attr_warning [%expr "-39"]]
         (Pat.constraint_ eq_var out_type)
         (Ppx_deriving.sanitize ~quoter (polymorphize comparator))]

let () =
  Ppx_deriving.(register (create deriver
    ~core_type: (Ppx_deriving.with_quoter expr_of_typ)
    ~type_decl_str: (fun ~options ~path type_decls ->
       [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path) type_decls))])
    ~type_decl_sig: (fun ~options ~path type_decls ->
       List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
