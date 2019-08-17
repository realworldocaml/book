open Import
open Ast_helper
open Printf

let apply_parsers funcs args types =
  List.fold_right2 (List.combine funcs args) types ~init:(M.expr "k")
    ~f:(fun (func, arg) typ acc ->
      match typ.ptyp_desc with
      | Ptyp_constr (path, _) when is_loc path.txt ->
        M.expr "let k = %a ctx %a.loc %a.txt k in %a"
          A.expr (evar func)
          A.expr arg
          A.expr arg
          A.expr acc
      | _ ->
        M.expr "let k = %a ctx loc %a k in %a"
          A.expr (evar func)
          A.expr arg
          A.expr acc)
;;

let assert_no_attributes ~path ~prefix =
  M.expr "Common.assert_no_attributes x.%a"
    A.id (fqn_longident' path (prefix ^ "attributes"))

let gen_combinator_for_constructor ?wrapper path ~prefix cd =
  match cd.pcd_args with
  | Pcstr_record _ -> failwith "Pcstr_record not supported"
  | Pcstr_tuple cd_args ->
    let args =
      List.mapi cd_args ~f:(fun i _ -> sprintf "x%d" i)
    in
    let funcs =
      List.mapi cd_args ~f:(fun i _ -> sprintf "f%d" i)
    in
    let pat =
      Pat.construct (Loc.mk (fqn_longident path cd.pcd_name.txt))
        (match args with
         | []  -> None
         | [x] -> Some (pvar x)
         | _   -> Some (Pat.tuple (List.map args ~f:pvar)))
    in
    let exp =
      apply_parsers funcs (List.map args ~f:evar) cd_args
    in
    let expected = without_prefix ~prefix cd.pcd_name.txt in
    let body =
      M.expr
        {|match x with
          | %a -> ctx.matched <- ctx.matched + 1; %a
          | _ -> fail loc %S|}
        A.patt pat
        A.expr exp
        expected
    in
    let body =
      match wrapper with
      | None -> body
      | Some (path, prefix, has_attrs) ->
        let body =
          M.expr
            {|let loc = x.%a in
              let x = x.%a in
              %a|}
            A.id (fqn_longident' path (prefix ^ "loc"))
            A.id (fqn_longident' path (prefix ^ "desc"))
            A.expr body
        in
        if has_attrs then
          Exp.sequence (assert_no_attributes ~path ~prefix) body
        else
          body
    in
    let body =
      let loc =
        match wrapper with
        | None -> M.patt "loc"
        | Some _ -> M.patt "_loc"
      in
      M.expr "T (fun ctx %a x k -> %a)"
        A.patt loc
        A.expr body
    in
    let body =
      List.fold_right funcs ~init:body ~f:(fun func acc ->
        M.expr "fun (T %a) -> %a"
          A.patt (pvar func)
          A.expr acc)
    in
    M.stri "let %a = %a"
      A.patt (pvar (function_name_of_id ~prefix cd.pcd_name.txt))
      A.expr body
;;

let gen_combinator_for_record path ~prefix ~has_attrs lds =
  let fields = List.map lds ~f:(fun ld -> fqn_longident path ld.pld_name.txt) in
  let funcs =
    List.map lds ~f:(fun ld -> map_keyword (without_prefix ~prefix ld.pld_name.txt))
  in
  let body =
    apply_parsers funcs
      (List.map fields ~f:(fun field -> Exp.field (evar "x") (Loc.mk field)))
      (List.map lds ~f:(fun ld -> ld.pld_type))
  in
  let body =
    if has_attrs then
      Exp.sequence (assert_no_attributes ~path ~prefix) body
    else
      body
  in
  let body = M.expr "T (fun ctx loc x k -> %a)" A.expr body in
  let body =
    List.fold_right funcs ~init:body ~f:(fun func acc ->
      Exp.fun_ (Labelled func) None (M.patt "T %a" A.patt (pvar func)) acc)
  in
  M.stri "let %a = %a"
    A.patt (pvar (function_name_of_path path))
    A.expr body
;;

let prefix_of_record lds = common_prefix (List.map lds ~f:(fun ld -> ld.pld_name.txt))

let filter_labels ~prefix lds =
  List.filter lds ~f:(fun ld ->
    match without_prefix ~prefix ld.pld_name.txt with
    | "loc" | "attributes" -> false
    | _ -> true)
;;

let has_ld ~prefix lds label =
  List.exists lds ~f:(fun ld -> ld.pld_name.txt = prefix ^ label)
;;

let attributes_parser ~prefix ~name ~has_loc =
  let field s = Lident (prefix ^ s) in
  let body =
    M.expr
      {|let k = f1 ctx loc x.%a k in
        let x = { x with %a = [] } in
        let k = f2 ctx loc x k in
        k|}
      A.id (field "attributes")
      A.id (field "attributes")
  in
  let body =
    if has_loc then
      M.expr "let loc = x.%a in %a"
        A.id (field "loc")
        A.expr body
    else
      body
  in
  let loc_patt =
    if has_loc then M.patt "_loc" else M.patt "loc"
  in
  M.stri
    "let %a (T f1) (T f2) = T (fun ctx %a x k -> %a)"
    A.patt (pvar name)
    A.patt loc_patt
    A.expr body

let gen_td ?wrapper path td =
  if is_loc path then
    []
  else
    match td.ptype_kind with
    | Ptype_variant cds -> begin
        let prefix =
          common_prefix (List.map cds ~f:(fun cd -> cd.pcd_name.txt))
        in
        let items =
          List.map cds ~f:(fun cd ->
            gen_combinator_for_constructor ?wrapper path ~prefix cd)
        in
        match wrapper with
        | Some (_, prefix, has_attrs) ->
          let field s = Exp.field (evar "x") (Loc.lident @@ prefix ^ s) in
          let items =
            if has_attrs then
              attributes_parser ~has_loc:true ~prefix ~name:(prefix ^ "attributes")
              :: items
            else
              items
          in
          M.stri
            {|let %a = fun (T f1) (T f2) ->
              T (fun ctx _loc x k ->
                let loc = %a in
                let k = f1 ctx loc loc k in
                let k = f2 ctx loc x k in
                k
              )|}
            A.patt (pvar @@ prefix ^ "loc")
            A.expr (field "loc")
          :: items
        | _ -> items
      end
    | Ptype_record lds ->
      let prefix = prefix_of_record lds in
      let has_attrs = has_ld ~prefix lds "attributes" in
      let has_loc   = has_ld ~prefix lds "loc" in
      let lds = filter_labels ~prefix lds in
      let items = [gen_combinator_for_record path ~prefix ~has_attrs lds] in
      if has_attrs then
        attributes_parser ~has_loc ~prefix
          ~name:(function_name_of_path path ^ "_attributes")
        :: items
      else
        items
    | Ptype_abstract | Ptype_open -> []
;;

let is_abstract td =
  match td.ptype_kind with
  | Ptype_abstract -> true
  | _ -> false
;;

let dump fn ~ext printer x =
  let oc = open_out (fn ^ ext) in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "%a@." printer x;
  close_out oc

let generate filename =
  let types = get_types ~filename in
  let types_with_wrapped =
    List.map types ~f:(fun (path, td) ->
      match td.ptype_kind with
      | Ptype_record lds ->
        let prefix = prefix_of_record lds in
        let lds' = filter_labels ~prefix lds in
        (match is_wrapper ~prefix lds' with
         | None -> (path, td, None)
         | Some p ->
           let has_attrs = has_ld ~prefix lds "attributes" in
           (path, td, Some (prefix, has_attrs, p.txt)))
      | _ -> (path, td, None))
  in
  let wrapped =
    List.filter_map types_with_wrapped ~f:(fun (_, _, x) ->
      match x with
      | None -> None
      | Some (_, _, p) -> Some p)
  in
  let types =
    List.filter types_with_wrapped ~f:(fun (path, _, _) ->
      not (List.mem path ~set:wrapped))
    |> List.map ~f:(fun (path, td, wrapped) ->
      match wrapped with
      | None -> (path, td, None)
      | Some (prefix, has_attrs, p) ->
        (path, td, Some (prefix, has_attrs, p, List.assoc p types)))
  in
  (*  let all_types = List.map fst types in*)
  let types =
    List.sort types ~cmp:(fun (a, _, _) (b, _, _) ->
      compare a b)
  in
  let items =
    List.map types ~f:(fun (path, td, wrapped) ->
      if is_abstract td then
        []
      else
        match wrapped with
        | None -> gen_td path td
        | Some (prefix, has_attrs, path', td') ->
          gen_td ~wrapper:(path, prefix, has_attrs) path' td'
    )
    |> List.flatten
  in
  let st =
    Str.open_ (Opn.mk (Loc.lident "Import"))
    :: Str.open_ (Opn.mk (Loc.lident "Ast_pattern0"))
    :: items
  in
  dump "ast_pattern_generated" Pprintast.structure st ~ext:".ml"

let args =
  [ ]

let usage = Printf.sprintf "%s [options] <.ml files>\n" Sys.argv.(0)

let () =
  let fns = ref [] in
  Arg.parse (Arg.align args) (fun fn -> fns := fn :: !fns) usage;
  try
    List.iter (List.rev !fns) ~f:generate
  with exn ->
    Errors.report_error Format.err_formatter exn;
    exit 2
