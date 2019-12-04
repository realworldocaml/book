open! Import

include Clause_syntax_intf

module Variant = struct
  type ast = constructor_declaration

  type t = { ast : ast; position : int }

  let create_list list =
    List.mapi list ~f:(fun position ast ->
      let loc = ast.pcd_loc in
      match ast.pcd_res with
      | Some _ -> unsupported ~loc "GADT"
      | None -> { ast; position })

  let salt t = Some t.position

  let location t = t.ast.pcd_loc

  let core_type_list t =
    match t.ast.pcd_args with
    | Pcstr_tuple list -> list
    | Pcstr_record label_decl_list ->
      List.map label_decl_list ~f:(fun label_decl ->
        label_decl.pld_type)

  let pattern t ~loc pat_list =
    let arg =
      match t.ast.pcd_args with
      | Pcstr_tuple _ ->
        begin
          match pat_list with
          | []    -> None
          | [pat] -> Some pat
          | _     -> Some (ppat_tuple ~loc pat_list)
        end
      | Pcstr_record label_decl_list ->
        let alist =
          List.map2_exn label_decl_list pat_list ~f:(fun label_decl pat ->
            lident_loc label_decl.pld_name, pat)
        in
        Some (ppat_record ~loc alist Closed)
    in
    ppat_construct ~loc (lident_loc t.ast.pcd_name) arg

  let expression t ~loc _ expr_list =
    let arg =
      match t.ast.pcd_args with
      | Pcstr_tuple _ ->
        begin
          match expr_list with
          | []     -> None
          | [expr] -> Some expr
          | _      -> Some (pexp_tuple ~loc expr_list)
        end
      | Pcstr_record label_decl_list ->
        let alist =
          List.map2_exn label_decl_list expr_list ~f:(fun label_decl expr ->
            lident_loc label_decl.pld_name, expr)
        in
        Some (pexp_record ~loc alist None)
    in
    pexp_construct ~loc (lident_loc t.ast.pcd_name) arg
end

module Polymorphic_variant = struct
  type ast = row_field

  type t = ast

  let create_list = Fn.id

  let salt t =
    match t with
    | Rtag (label, _, _, _) -> Some (Ocaml_common.Btype.hash_variant label.txt)
    | Rinherit _ -> None

  let location t =
    match t with
    | Rtag (label, _, _, _) -> label.loc
    | Rinherit core_type -> core_type.ptyp_loc

  let core_type_list t =
    match t with
    | Rtag (_, _, _, core_type_list) -> core_type_list
    | Rinherit core_type -> [core_type]

  let pattern t ~loc pat_list =
    match t, pat_list with
    | Rtag (label, _, true, []), [] ->
      ppat_variant ~loc label.txt None
    | Rtag (label, _, false, [_]), [pat] ->
      ppat_variant ~loc label.txt (Some pat)
    | Rtag (label, _, false, [_]), (_ :: _ :: _) ->
      ppat_variant ~loc label.txt (Some (ppat_tuple ~loc pat_list))
    | Rinherit { ptyp_desc; _ }, [{ ppat_desc; _ }] ->
      begin
        match ptyp_desc with
        | Ptyp_constr (id, []) ->
          begin
            match ppat_desc with
            | Ppat_var var -> ppat_alias ~loc (ppat_type ~loc id) var
            | _ ->
              internal_error ~loc
                "cannot bind a #<type> pattern to anything other than a variable"
          end
        | _ ->
          unsupported ~loc "inherited polymorphic variant type that is not a type name"
      end
    | Rtag (_, _, true, _ :: _), _
    | Rtag (_, _, false, ([] | _ :: _ :: _)), _ ->
      unsupported ~loc "intersection type"
    | Rtag (_, _, true, []), _ :: _
    | Rtag (_, _, false, [_]), []
    | Rinherit _, ([] | _ :: _ :: _) ->
      internal_error ~loc "wrong number of arguments for variant clause"

  let expression t ~loc core_type expr_list =
    match t, expr_list with
    | Rtag (label, _, true, []), [] ->
      pexp_variant ~loc label.txt None
    | Rtag (label, _, false, [_]), [expr] ->
      pexp_variant ~loc label.txt (Some expr)
    | Rtag (label, _, false, [_]), (_ :: _ :: _) ->
      pexp_variant ~loc label.txt (Some (pexp_tuple ~loc expr_list))
    | Rinherit inherited_type, [expr] ->
      pexp_coerce ~loc expr (Some inherited_type) core_type
    | Rtag (_, _, true, _ :: _), _
    | Rtag (_, _, false, ([] | _ :: _ :: _)), _ ->
      unsupported ~loc "intersection type"
    | Rtag (_, _, true, []), _ :: _
    | Rtag (_, _, false, [_]), []
    | Rinherit _, ([] | _ :: _ :: _) ->
      internal_error ~loc "wrong number of arguments for variant clause"
end
