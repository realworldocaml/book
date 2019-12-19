#include "../compat_macros.cppo"

open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "show"
let raise_errorf = Ppx_deriving.raise_errorf

type options = { with_path : bool }

(* The option [with_path] controls whether a full path should be displayed
   as part of data constructor names and record field names. (In the case
   of record fields, it is displayed only as part of the name of the first
   field.) By default, this option is [true], which means that full paths
   are shown. *)

let expand_path show_opts ~path name =
  let path = if show_opts.with_path then path else [] in
  Ppx_deriving.expand_path ~path name

let parse_options options =
  let with_path = ref true in
  options |> List.iter (fun (name, expr) ->
    match name with
    | "with_path" -> with_path := Ppx_deriving.Arg.(get_expr ~deriver bool) expr
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name);
  { with_path = !with_path }

let attr_nobuiltin attrs =
  Ppx_deriving.(attrs |> attr ~deriver "nobuiltin" |> Arg.get_flag ~deriver)

let attr_printer attrs =
  Ppx_deriving.(attrs |> attr ~deriver "printer" |> Arg.(get_attr ~deriver expr))

let attr_polyprinter attrs =
  Ppx_deriving.(attrs |> attr ~deriver "polyprinter" |> Arg.(get_attr ~deriver expr))

let attr_opaque attrs =
  Ppx_deriving.(attrs |> attr ~deriver "opaque" |> Arg.get_flag ~deriver)

let argn = Printf.sprintf "a%d"
let argl = Printf.sprintf "a%s"

let pattn typs   = List.mapi (fun i _ -> pvar (argn i)) typs
let pattl labels = List.map (fun { pld_name = { txt = n } } -> n, pvar (argl n)) labels

let pconstrrec name fields = pconstr name [precord ~closed:Closed fields]

let wrap_printer quoter printer =
  Ppx_deriving.quote quoter
    [%expr (let fprintf = Ppx_deriving_runtime.Format.fprintf in [%e printer]) [@ocaml.warning "-26"]]

let pp_type_of_decl ~options ~path type_decl =
  let _ = parse_options options in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: Ppx_deriving_runtime.Format.formatter -> [%t var] -> Ppx_deriving_runtime.unit])
    type_decl
    [%type: Ppx_deriving_runtime.Format.formatter -> [%t typ] -> Ppx_deriving_runtime.unit]

let show_type_of_decl ~options ~path type_decl =
  let _ = parse_options options in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: Ppx_deriving_runtime.Format.formatter -> [%t var] -> Ppx_deriving_runtime.unit])
    type_decl
    [%type: [%t typ] -> Ppx_deriving_runtime.string]

let sig_of_type ~options ~path type_decl =
  let _ = parse_options options in
  [Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl))
              (pp_type_of_decl ~options ~path type_decl));
   Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Prefix "show") type_decl))
              (show_type_of_decl ~options ~path type_decl))]

let rec expr_of_typ quoter typ =
  let expr_of_typ = expr_of_typ quoter in
  match attr_printer typ.ptyp_attributes with
  | Some printer -> [%expr [%e wrap_printer quoter printer] fmt]
  | None ->
  if attr_opaque typ.ptyp_attributes then
    [%expr fun _ -> Ppx_deriving_runtime.Format.pp_print_string fmt "<opaque>"]
  else
    let format x = [%expr Ppx_deriving_runtime.Format.fprintf fmt [%e str x]] in
    let seq start finish fold typ =
      [%expr fun x ->
        Ppx_deriving_runtime.Format.fprintf fmt [%e str start];
        ignore ([%e fold] (fun sep x ->
          if sep then Ppx_deriving_runtime.Format.fprintf fmt ";@ ";
          [%e expr_of_typ typ] x; true) false x);
        Ppx_deriving_runtime.Format.fprintf fmt [%e str finish];]
    in
    let typ = Ppx_deriving.remove_pervasives ~deriver typ in
    match typ with
    | [%type: _] -> [%expr fun _ -> Ppx_deriving_runtime.Format.pp_print_string fmt "_"]
    | { ptyp_desc = Ptyp_arrow _ } ->
      [%expr fun _ -> Ppx_deriving_runtime.Format.pp_print_string fmt "<fun>"]
    | { ptyp_desc = Ptyp_constr _ } ->
      let builtin = not (attr_nobuiltin typ.ptyp_attributes) in
      begin match builtin, typ with
      | true, [%type: unit]        -> [%expr fun () -> Ppx_deriving_runtime.Format.pp_print_string fmt "()"]
      | true, [%type: int]         -> format "%d"
      | true, [%type: int32]
      | true, [%type: Int32.t]     -> format "%ldl"
      | true, [%type: int64]
      | true, [%type: Int64.t]     -> format "%LdL"
      | true, [%type: nativeint]
      | true, [%type: Nativeint.t] -> format "%ndn"
      | true, [%type: float]       -> format "%F"
      | true, [%type: bool]        -> format "%B"
      | true, [%type: char]        -> format "%C"
      | true, [%type: string]
      | true, [%type: String.t]    -> format "%S"
      | true, [%type: bytes]
      | true, [%type: Bytes.t] ->
        [%expr fun x -> Ppx_deriving_runtime.Format.fprintf fmt "%S" (Bytes.to_string x)]
      | true, [%type: [%t? typ] ref] ->
        [%expr fun x ->
          Ppx_deriving_runtime.Format.pp_print_string fmt "ref (";
          [%e expr_of_typ typ] !x;
          Ppx_deriving_runtime.Format.pp_print_string fmt ")"]
      | true, [%type: [%t? typ] list]  -> seq "@[<2>["   "@,]@]" [%expr List.fold_left]  typ
      | true, [%type: [%t? typ] array] -> seq "@[<2>[|" "@,|]@]" [%expr Array.fold_left] typ
      | true, [%type: [%t? typ] option] ->
        [%expr
          function
          | None -> Ppx_deriving_runtime.Format.pp_print_string fmt "None"
          | Some x ->
            Ppx_deriving_runtime.Format.pp_print_string fmt "(Some ";
            [%e expr_of_typ typ] x;
            Ppx_deriving_runtime.Format.pp_print_string fmt ")"]
      | true, ([%type: ([%t? ok_t], [%t? err_t]) result] |
               [%type: ([%t? ok_t], [%t? err_t]) Result.result]) ->
        [%expr
          function
          | Result.Ok ok ->
            Ppx_deriving_runtime.Format.pp_print_string fmt "(Ok ";
            [%e expr_of_typ ok_t] ok;
            Ppx_deriving_runtime.Format.pp_print_string fmt ")"
          | Result.Error e ->
            Ppx_deriving_runtime.Format.pp_print_string fmt "(Error ";
            [%e expr_of_typ err_t] e;
            Ppx_deriving_runtime.Format.pp_print_string fmt ")"]
      | true, ([%type: [%t? typ] lazy_t] | [%type: [%t? typ] Lazy.t]) ->
        [%expr fun x ->
          if Lazy.is_val x then [%e expr_of_typ typ] (Lazy.force x)
          else Ppx_deriving_runtime.Format.pp_print_string fmt "<not evaluated>"]
      | _, { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
        let args_pp = List.map (fun typ -> [%expr fun fmt -> [%e expr_of_typ typ]]) args in
        let printer =
          match attr_polyprinter typ.ptyp_attributes with
          | Some printer -> wrap_printer quoter printer
          | None ->
            let printer = Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Prefix "pp") lid)) in
            Ppx_deriving.quote quoter printer
        in
        app printer (args_pp @ [[%expr fmt]])
      | _ -> assert false
      end
    | { ptyp_desc = Ptyp_tuple typs } ->
      let args = List.mapi (fun i typ -> app (expr_of_typ typ) [evar (argn i)]) typs in
      [%expr
        fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
        Ppx_deriving_runtime.Format.fprintf fmt "(@[";
        [%e args |> Ppx_deriving.(fold_exprs
                (seq_reduce ~sep:[%expr Ppx_deriving_runtime.Format.fprintf fmt ",@ "]))];
        Ppx_deriving_runtime.Format.fprintf fmt "@])"]
    | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
      let cases =
        fields |> List.map (fun field ->
          match field with
          | Rtag_patt(label, true (*empty*), []) ->
#if OCAML_VERSION >= (4, 06, 0)
            let label = label.txt in
#endif
            Exp.case (Pat.variant label None)
                     [%expr Ppx_deriving_runtime.Format.pp_print_string fmt [%e str ("`" ^ label)]]
          | Rtag_patt(label, false, [typ]) ->
#if OCAML_VERSION >= (4, 06, 0)
            let label = label.txt in
#endif
            Exp.case (Pat.variant label (Some [%pat? x]))
                     [%expr Ppx_deriving_runtime.Format.fprintf fmt [%e str ("`" ^ label ^ " (@[<hov>")];
                            [%e expr_of_typ typ] x;
                            Ppx_deriving_runtime.Format.fprintf fmt "@])"]
          | Rinherit_patt({ ptyp_desc = Ptyp_constr (tname, _) } as typ) ->
            Exp.case [%pat? [%p Pat.type_ tname] as x]
                     [%expr [%e expr_of_typ typ] x]
          | _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                         deriver (Ppx_deriving.string_of_core_type typ))
      in
      Exp.function_ cases
    | { ptyp_desc = Ptyp_var name } -> [%expr [%e evar ("poly_"^name)] fmt]
    | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
    | { ptyp_loc } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                   deriver (Ppx_deriving.string_of_core_type typ)

and expr_of_label_decl quoter { pld_type; pld_attributes } =
  let attrs = pld_type.ptyp_attributes @ pld_attributes in
  expr_of_typ quoter { pld_type with ptyp_attributes = attrs }

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let show_opts = parse_options options in
  let quoter = Ppx_deriving.create_quoter () in
  let path = Ppx_deriving.path_of_type_decl ~path type_decl in
  let prettyprinter =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest ->
      [%expr fun fmt -> [%e expr_of_typ quoter manifest]]
    | Ptype_variant constrs, _ ->
      let cases =
        constrs |> List.map (fun { pcd_name = { txt = name' }; pcd_args; pcd_attributes } ->
          let constr_name =
            expand_path show_opts ~path name'
          in

          match attr_printer pcd_attributes, pcd_args with
          | Some printer, Pcstr_tuple(args) ->
            let rec range from_idx to_idx =
              if from_idx = to_idx
              then []
              else from_idx::(range (from_idx+1) to_idx)
            in
            let indices = range 0 (List.length args) in
            let pattern_vars =
              List.map (fun i -> pvar ("a" ^ string_of_int i)) indices
            in
            let expr_vars =
              List.map (fun i -> evar ("a" ^ string_of_int i)) indices
            in
            Exp.case (pconstr name' pattern_vars)
              [%expr [%e wrap_printer quoter printer] fmt
                        [%e tuple expr_vars]]
#if OCAML_VERSION >= (4, 03, 0)
          | Some printer, Pcstr_record(labels) ->
            let args = labels |> List.map (fun { pld_name = { txt = n } } -> evar (argl n)) in
            Exp.case (pconstrrec name' (pattl labels))
                     (app (wrap_printer quoter printer) ([%expr fmt] :: args))
#endif
          | None, Pcstr_tuple(typs) ->
            let args =
              List.mapi (fun i typ -> app (expr_of_typ quoter typ) [evar (argn i)]) typs in
            let printer =
              match args with
              | []   -> [%expr Ppx_deriving_runtime.Format.pp_print_string fmt [%e str constr_name]]
              | [arg] ->
                [%expr
                  Ppx_deriving_runtime.Format.fprintf fmt [%e str ("(@[<2>" ^  constr_name ^ "@ ")];
                  [%e arg];
                  Ppx_deriving_runtime.Format.fprintf fmt "@])"]
              | args ->
                [%expr
                  Ppx_deriving_runtime.Format.fprintf fmt [%e str ("(@[<2>" ^  constr_name ^ " (@,")];
                  [%e args |> Ppx_deriving.(fold_exprs
                        (seq_reduce ~sep:[%expr Ppx_deriving_runtime.Format.fprintf fmt ",@ "]))];
                  Ppx_deriving_runtime.Format.fprintf fmt "@,))@]"]
            in
            Exp.case (pconstr name' (pattn typs)) printer
#if OCAML_VERSION >= (4, 03, 0)
          | None, Pcstr_record(labels) ->
            let args =
              labels |> List.map (fun ({ pld_name = { txt = n }; _ } as pld) ->
                [%expr
                  Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ " [%e str n];
                  [%e expr_of_label_decl quoter pld]
                    [%e evar (argl n)];
                  Ppx_deriving_runtime.Format.fprintf fmt "@]"
                ])
            in
            let printer =
              [%expr
                Ppx_deriving_runtime.Format.fprintf fmt [%e str ("@[<2>" ^  constr_name ^ " {@,")];
                [%e args |> Ppx_deriving.(fold_exprs
                      (seq_reduce ~sep:[%expr Ppx_deriving_runtime.Format.fprintf fmt ";@ "]))];
                Ppx_deriving_runtime.Format.fprintf fmt "@]}"]
            in
            Exp.case (pconstrrec name' (pattl labels)) printer
#endif
          )
      in
      [%expr fun fmt -> [%e Exp.function_ cases]]
    | Ptype_record labels, _ ->
      let fields =
        labels |> List.mapi (fun i ({ pld_name = { txt = name }; _} as pld) ->
          let field_name = if i = 0 then expand_path show_opts ~path name else name in
          [%expr
            Ppx_deriving_runtime.Format.fprintf fmt "@[%s =@ " [%e str field_name];
            [%e expr_of_label_decl quoter pld]
              [%e Exp.field (evar "x") (mknoloc (Lident name))];
            Ppx_deriving_runtime.Format.fprintf fmt "@]"
          ])
      in
      [%expr fun fmt x ->
        Ppx_deriving_runtime.Format.fprintf fmt "@[<2>{ ";
        [%e fields |> Ppx_deriving.(fold_exprs
              (seq_reduce ~sep:[%expr Ppx_deriving_runtime.Format.fprintf fmt ";@ "]))];
        Ppx_deriving_runtime.Format.fprintf fmt "@ }@]"]
    | Ptype_abstract, None ->
      raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
    | Ptype_open, _        ->
      raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let pp_poly_apply = Ppx_deriving.poly_apply_of_type_decl type_decl (evar
                        (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl)) in
  let stringprinter = [%expr fun x -> Ppx_deriving_runtime.Format.asprintf "%a" [%e pp_poly_apply] x] in
  let polymorphize  = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let pp_type =
    Ppx_deriving.strong_type_of_type @@ pp_type_of_decl ~options ~path type_decl in
  let show_type =
    Ppx_deriving.strong_type_of_type @@
      show_type_of_decl ~options ~path type_decl in
  let pp_var =
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "pp") type_decl) in
  let show_var =
    pvar (Ppx_deriving.mangle_type_decl (`Prefix "show") type_decl) in
  let no_warn_32 = Ppx_deriving.attr_warning [%expr "-32"] in
  [Vb.mk (Pat.constraint_ pp_var pp_type)
         (Ppx_deriving.sanitize ~quoter (polymorphize prettyprinter));
   Vb.mk ~attrs:[no_warn_32] (Pat.constraint_ show_var show_type) (polymorphize stringprinter);]

let () =
  Ppx_deriving.(register (create deriver
    ~core_type: (Ppx_deriving.with_quoter (fun quoter typ ->
      [%expr fun x -> Ppx_deriving_runtime.Format.asprintf "%a" (fun fmt -> [%e expr_of_typ quoter typ]) x]))
    ~type_decl_str: (fun ~options ~path type_decls ->
      [Str.value Recursive (List.concat (List.map (str_of_type ~options ~path) type_decls))])
    ~type_decl_sig: (fun ~options ~path type_decls ->
      List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
