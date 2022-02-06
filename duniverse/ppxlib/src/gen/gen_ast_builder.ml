open Import
open Ast_helper
open Printf

let prefix_of_record lds =
  common_prefix (List.map lds ~f:(fun ld -> ld.pld_name.txt))

module Gen (Fixed_loc : sig
  val fixed_loc : bool
end) =
struct
  open Fixed_loc

  let gen_combinator_for_constructor
      ~wrapper:(wpath, wprefix, has_attrs, has_loc_stack) path ~prefix cd =
    match cd.pcd_args with
    | Pcstr_record _ ->
        (* TODO. *)
        failwith "Pcstr_record not supported"
    | Pcstr_tuple cd_args ->
        let args = List.mapi cd_args ~f:(fun i _ -> sprintf "x%d" i) in
        let exp =
          Exp.construct
            (Loc.mk (fqn_longident path cd.pcd_name.txt))
            (match args with
            | [] -> None
            | [ x ] -> Some (evar x)
            | _ -> Some (Exp.tuple (List.map args ~f:evar)))
        in
        let body =
          let fields =
            [
              (Loc.mk (fqn_longident' wpath (wprefix ^ "loc")), evar "loc");
              (Loc.mk (fqn_longident' wpath (wprefix ^ "desc")), exp);
            ]
          in
          let fields =
            if has_attrs then
              ( Loc.mk (fqn_longident' wpath (wprefix ^ "attributes")),
                M.expr "[]" )
              :: fields
            else fields
          in
          let fields =
            if has_loc_stack then
              ( Loc.mk (fqn_longident' wpath (wprefix ^ "loc_stack")),
                M.expr "[]" )
              :: fields
            else fields
          in
          Exp.record fields None
        in
        let body =
          (* match args with
             | [] -> [%expr fun () -> [%e body]]
             | _ ->*)
          List.fold_right args ~init:body ~f:(fun arg acc ->
              M.expr "fun %a -> %a" A.patt (pvar arg) A.expr acc)
        in
        (* let body =
             if not has_attrs then
               body
             else
               [%expr fun ?(attrs=[]) -> [%e body]]
           in*)
        let body =
          if fixed_loc then body else M.expr "fun ~loc -> %a" A.expr body
        in
        M.stri "let %a = %a" A.patt
          (pvar (function_name_of_id ~prefix cd.pcd_name.txt))
          A.expr body

  let gen_combinator_for_record path ~prefix lds =
    let fields =
      List.map lds ~f:(fun ld -> fqn_longident path ld.pld_name.txt)
    in
    let funcs =
      List.map lds ~f:(fun ld ->
          map_keyword (without_prefix ~prefix ld.pld_name.txt))
    in
    let body =
      Exp.record
        (List.map2 fields funcs ~f:(fun field func ->
             ( Loc.mk field,
               if func = "attributes" then M.expr "[]" else evar func )))
        None
    in
    let body =
      let l = List.filter funcs ~f:(fun f -> f <> "loc" && f <> "attributes") in
      match l with
      | [ x ] -> Exp.fun_ Nolabel None (pvar x) body
      | _ ->
          List.fold_right l ~init:body ~f:(fun func acc ->
              Exp.fun_ (Labelled func) None (pvar func) acc)
    in
    (* let body =
         if List.mem "attributes" ~set:funcs then
           [%expr fun ?(attrs=[]) -> [%e body]]
         else
           body
       in*)
    let body =
      if List.mem "loc" ~set:funcs && not fixed_loc then
        M.expr "fun ~loc -> %a" A.expr body
      else body
    in
    M.stri "let %a = %a" A.patt (pvar (function_name_of_path path)) A.expr body

  let gen_td ?wrapper path td =
    if is_loc path then []
    else
      match td.ptype_kind with
      | Ptype_variant cds -> (
          match wrapper with
          | None -> []
          | Some wrapper ->
              let prefix =
                common_prefix (List.map cds ~f:(fun cd -> cd.pcd_name.txt))
              in
              List.map cds ~f:(fun cd ->
                  gen_combinator_for_constructor ~wrapper path ~prefix cd))
      | Ptype_record lds ->
          let prefix = prefix_of_record lds in
          [ gen_combinator_for_record path ~prefix lds ]
      | Ptype_abstract | Ptype_open -> []
end

let filter_labels ~prefix lds =
  List.filter lds ~f:(fun ld ->
      match without_prefix ~prefix ld.pld_name.txt with
      | "loc" | "loc_stack" | "attributes" -> false
      | _ -> true)

let is_abstract td =
  match td.ptype_kind with Ptype_abstract -> true | _ -> false

let dump fn ~ext printer x =
  let oc = open_out (fn ^ ext) in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "%a@." printer x;
  close_out oc

let generate filename =
  (*  let fn = Misc.find_in_path_uncap !Config.load_path (unit ^ ".cmi") in*)
  let types = get_types ~filename in
  let types_with_wrapped =
    List.map types ~f:(fun (path, td) ->
        match td.ptype_kind with
        | Ptype_record lds -> (
            let prefix = prefix_of_record lds in
            let lds' = filter_labels ~prefix lds in
            match is_wrapper ~prefix lds' with
            | None -> (path, td, None)
            | Some p ->
                let has_attrs =
                  List.exists lds ~f:(fun ld ->
                      ld.pld_name.txt = prefix ^ "attributes")
                in
                let has_loc_stack =
                  List.exists lds ~f:(fun ld ->
                      ld.pld_name.txt = prefix ^ "loc_stack")
                in
                (path, td, Some (prefix, has_attrs, has_loc_stack, p.txt)))
        | _ -> (path, td, None))
  in
  let wrapped =
    List.filter_map types_with_wrapped ~f:(fun (_, _, x) ->
        match x with None -> None | Some (_, _, _, p) -> Some p)
  in
  let types =
    List.filter types_with_wrapped ~f:(fun (path, _, _) ->
        not (List.mem path ~set:wrapped))
    |> List.map ~f:(fun (path, td, wrapped) ->
           match wrapped with
           | None -> (path, td, None)
           | Some (prefix, has_attrs, has_loc_stack, p) ->
               ( path,
                 td,
                 Some (prefix, has_attrs, has_loc_stack, p, List.assoc p types)
               ))
  in
  (*  let all_types = List.map fst types in*)
  let types = List.sort types ~cmp:(fun (a, _, _) (b, _, _) -> compare a b) in
  let items fixed_loc =
    let module G = Gen (struct
      let fixed_loc = fixed_loc
    end) in
    List.map types ~f:(fun (path, td, wrapped) ->
        if is_abstract td then []
        else
          match wrapped with
          | None -> G.gen_td path td
          | Some (prefix, has_attrs, has_loc_stack, path', td') ->
              G.gen_td
                ~wrapper:(path, prefix, has_attrs, has_loc_stack)
                path' td')
    |> List.flatten
  in
  let st =
    [
      Str.open_ (Opn.mk (Mod.ident (Loc.lident "Import")));
      Str.module_ (Mb.mk (Loc.mk (Some "M")) (Mod.structure (items false)));
      Str.module_
        (Mb.mk (Loc.mk (Some "Make"))
           (Mod.functor_
              (Named
                 ( Loc.mk (Some "Loc"),
                   Mty.signature
                     [ Sig.value (Val.mk (Loc.mk "loc") (M.ctyp "Location.t")) ]
                 ))
              (Mod.structure (M.stri "let loc = Loc.loc" :: items true))));
    ]
  in
  dump "ast_builder_generated" Pprintast.structure st ~ext:".ml"

let args = []

let usage = Printf.sprintf "%s [options] <.ml files>\n" Sys.argv.(0)

let () =
  let fns = ref [] in
  Arg.parse (Arg.align args) (fun fn -> fns := fn :: !fns) usage;
  try List.iter (List.rev !fns) ~f:generate
  with exn ->
    Astlib.Location.report_exception Format.err_formatter exn;
    exit 2
