#include "../compat_macros.cppo"

open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "ord"
let raise_errorf = Ppx_deriving.raise_errorf

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

let attr_nobuiltin attrs =
  Ppx_deriving.(attrs |> attr ~deriver "nobuiltin" |> Arg.get_flag ~deriver)

let attr_compare attrs =
  Ppx_deriving.(attrs |> attr ~deriver "compare" |> Arg.(get_attr ~deriver expr))

let argn kind =
  Printf.sprintf (match kind with `lhs -> "lhs%d" | `rhs -> "rhs%d")

let argl kind =
  Printf.sprintf (match kind with `lhs -> "lhs%s" | `rhs -> "rhs%s")

let compare_reduce acc expr =
  [%expr match [%e expr] with 0 -> [%e acc] | x -> x]

let reduce_compare l =
  match List.rev l with
  | [] -> [%expr 0]
  | x :: xs -> List.fold_left compare_reduce x xs

let wildcard_case int_cases =
  Exp.case [%pat? _] [%expr
    let to_int = [%e Exp.function_ int_cases] in
    Ppx_deriving_runtime.compare (to_int lhs) (to_int rhs)]

let pattn side typs =
  List.mapi (fun i _ -> pvar (argn side i)) typs

let pattl side labels =
  List.map (fun { pld_name = { txt = n } } -> n, pvar (argl side n)) labels

let pconstrrec name fields =
  pconstr name [precord ~closed:Closed fields]

let rec exprn quoter typs =
  typs |> List.mapi (fun i typ ->
    app (expr_of_typ quoter typ) [evar (argn `lhs i); evar (argn `rhs i)])

and exprl quoter typs =
  typs |> List.map (fun ({ pld_name = { txt = n }; _ } as pld) ->
    app (expr_of_label_decl quoter pld)
      [evar (argl `lhs n); evar (argl `rhs n)])

and expr_of_label_decl quoter { pld_type; pld_attributes } =
  let attrs = pld_type.ptyp_attributes @ pld_attributes in
  expr_of_typ quoter { pld_type with ptyp_attributes = attrs }

and expr_of_typ quoter typ =
  let expr_of_typ = expr_of_typ quoter in
  match attr_compare typ.ptyp_attributes with
  | Some fn -> Ppx_deriving.quote quoter fn
  | None ->
    let typ = Ppx_deriving.remove_pervasives ~deriver typ in
    match typ with
    | [%type: _] -> [%expr fun _ _ -> 0]
    | { ptyp_desc = Ptyp_constr _ } ->
      let builtin = not (attr_nobuiltin typ.ptyp_attributes) in
      begin match builtin, typ with
      | true, [%type: _] ->
        [%expr fun _ _ -> 0]
      | true, [%type: unit] ->
        [%expr fun (_:unit) (_:unit) -> 0]
      | true, ([%type: int] | [%type: int32] | [%type: Int32.t]
              | [%type: int64] | [%type: Int64.t] | [%type: nativeint]
              | [%type: Nativeint.t] | [%type: float] | [%type: bool]
              | [%type: char] | [%type: string] | [%type: bytes]) ->
        let compare_fn = [%expr fun (a:[%t typ]) b -> Ppx_deriving_runtime.compare a b] in
        Ppx_deriving.quote quoter compare_fn
      | true, [%type: [%t? typ] ref] ->
        [%expr fun a b -> [%e expr_of_typ typ] !a !b]
      | true, [%type: [%t? typ] list]  ->
        [%expr
          let rec loop x y =
            match x, y with
            | [], [] -> 0
            | [], _ -> -1
            | _, [] -> 1
            | a :: x, b :: y ->
              [%e compare_reduce [%expr loop x y] [%expr [%e expr_of_typ typ] a b]]
          in (fun x y -> loop x y)]
      | true, [%type: [%t? typ] array] ->
        [%expr fun x y ->
          let rec loop i =
            if i = Array.length x then 0
            else [%e compare_reduce [%expr loop (i + 1)]
                                    [%expr [%e expr_of_typ typ] x.(i) y.(i)]]
          in
          [%e compare_reduce [%expr loop 0]
                             [%expr Ppx_deriving_runtime.compare (Array.length x) (Array.length y)]]]
      | true, [%type: [%t? typ] option] ->
        [%expr fun x y ->
          match x, y with
          | None, None -> 0
          | Some a, Some b -> [%e expr_of_typ typ] a b
          | None, Some _ -> -1
          | Some _, None -> 1]
      | true, ([%type: ([%t? ok_t], [%t? err_t]) result] |
               [%type: ([%t? ok_t], [%t? err_t]) Result.result]) ->
        [%expr fun x y ->
          match x, y with
          | Result.Error a, Result.Error b -> [%e expr_of_typ err_t] a b
          | Result.Ok a, Result.Ok b -> [%e expr_of_typ ok_t] a b
          | Result.Ok _ , Result.Error _ -> -1
          | Result.Error _ , Result.Ok _ -> 1]
      | true, ([%type: [%t? typ] lazy_t] | [%type: [%t? typ] Lazy.t]) ->
        [%expr fun (lazy x) (lazy y) -> [%e expr_of_typ typ] x y]
      | _, { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
        let compare_fn = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "compare") lid)) in
        let fwd = app (Ppx_deriving.quote quoter compare_fn) (List.map expr_of_typ args) in
        (* eta-expansion is necessary for recursive groups *)
        [%expr fun x -> [%e fwd] x]
      | _ -> assert false
      end
    | { ptyp_desc = Ptyp_tuple typs } ->
      [%expr fun [%p ptuple (pattn `lhs typs)] [%p ptuple (pattn `rhs typs)] ->
        [%e exprn quoter typs |> reduce_compare]]
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let variant label popt =
#if OCAML_VERSION < (4, 06, 0)
        Pat.variant label popt
#else
        Pat.variant label.txt popt
#endif
      in
      let cases =
        fields |> List.map (fun field ->
          let pdup f = ptuple [f "lhs"; f "rhs"] in
          match field with
          | Rtag_patt(label, true (*empty*), []) ->
            Exp.case (pdup (fun _ -> variant label None)) [%expr 0]
          | Rtag_patt(label, false, [typ]) ->
            Exp.case (pdup (fun var -> variant label (Some (pvar var))))
                     (app (expr_of_typ typ) [evar "lhs"; evar "rhs"])
          | Rinherit_patt({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
            Exp.case (pdup (fun var -> Pat.alias (Pat.type_ tname) (mknoloc var)))
                     (app (expr_of_typ typ) [evar "lhs"; evar "rhs"])
          | _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                         deriver (Ppx_deriving.string_of_core_type typ))
      in
      let int_cases =
        fields |> List.mapi (fun i field ->
          match field with
          | Rtag_patt(label, true (*empty*), []) ->
            Exp.case (variant label None) (int i)
          | Rtag_patt(label, false, [typ]) ->
            Exp.case (variant label (Some [%pat? _])) (int i)
          | Rinherit_patt({ ptyp_desc = Ptyp_constr (tname, []) }) ->
            Exp.case (Pat.type_ tname) (int i)
          | _ -> assert false)
      in
      [%expr fun lhs rhs ->
        [%e Exp.match_ [%expr lhs, rhs] (cases @ [wildcard_case int_cases])]]
    | { ptyp_desc = Ptyp_var name } -> evar ("poly_"^name)
    | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
    | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ)

let core_type_of_decl ~options ~path type_decl =
  parse_options options;
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
          (fun var -> [%type: [%t var] -> [%t var] -> Ppx_deriving_runtime.int]) type_decl in
  (polymorphize [%type: [%t typ] -> [%t typ] -> Ppx_deriving_runtime.int])

let sig_of_type ~options ~path type_decl =
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "compare") type_decl))
             (core_type_of_decl ~options ~path type_decl))]

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  parse_options options;
  let quoter = Ppx_deriving.create_quoter () in
  let comparator =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> expr_of_typ quoter manifest
    | Ptype_variant constrs, _ ->
      let int_cases =
          constrs |> List.mapi (fun i { pcd_name = { txt = name }; pcd_args } ->
            match pcd_args with
            | Pcstr_tuple([]) -> Exp.case (pconstr name []) (int i)
            | _               -> Exp.case (pconstr name [[%pat? _]]) (int i))
      and cases =
        constrs |> List.map (fun { pcd_name = { txt = name }; pcd_args } ->
          match pcd_args with
          | Pcstr_tuple(typs) ->
            exprn quoter typs |> reduce_compare |>
            Exp.case (ptuple [pconstr name (pattn `lhs typs);
                              pconstr name (pattn `rhs typs)])
#if OCAML_VERSION >= (4, 03, 0)
          | Pcstr_record(labels) ->
            exprl quoter labels |> reduce_compare |>
            Exp.case (ptuple [pconstrrec name (pattl `lhs labels);
                              pconstrrec name (pattl `rhs labels)])
#endif
          )
      in
      [%expr fun lhs rhs ->
        [%e Exp.match_ [%expr lhs, rhs] (cases @ [wildcard_case int_cases])]]
    | Ptype_record labels, _ ->
      let exprs =
        labels |> List.map (fun ({ pld_name = { txt = name }; _ } as pld) ->
          let field obj = Exp.field obj (mknoloc (Lident name)) in
          app (expr_of_label_decl quoter pld)
            [field (evar "lhs"); field (evar "rhs")])
      in
      [%expr fun lhs rhs -> [%e reduce_compare exprs]]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _ ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let out_type =
    Ppx_deriving.strong_type_of_type @@
      core_type_of_decl ~options ~path type_decl in
  let out_var =
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "compare") type_decl) in
  [Vb.mk ~attrs:[Ppx_deriving.attr_warning [%expr "-39"]]
         (Pat.constraint_ out_var out_type)
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
