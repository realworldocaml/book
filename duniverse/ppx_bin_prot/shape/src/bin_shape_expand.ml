open Base
open Ppxlib
open Ast_builder.Default

let raise_errorf ~loc fmt =
  Location.raise_errorf ~loc (Caml.(^^) "ppx_bin_shape: " fmt)

let loc_string loc =
  [%expr Bin_prot.Shape.Location.of_string
           [%e Ppx_here_expander.lift_position_as_string ~loc]]

let app_list ~loc (func:expression) (args:expression list) =
  [%expr [%e func] [%e elist ~loc args]]

let curry_app_list ~loc (func:expression) (args:expression list) =
  List.fold_left args ~init:func ~f:(fun acc arg -> [%expr [%e acc] [%e arg]])

let bin_shape_ tname = "bin_shape_" ^ tname

let bin_shape_lid ~loc id =
  unapplied_type_constr_conv ~loc id ~f:bin_shape_

let shape_tid ~loc ~(tname:string) =
  [%expr Bin_prot.Shape.Tid.of_string [%e estring ~loc tname]]

let shape_vid ~loc ~(tvar:string) =
  [%expr Bin_prot.Shape.Vid.of_string [%e estring ~loc tvar]]

let shape_rec_app ~loc ~(tname:string) =
  [%expr Bin_prot.Shape.rec_app [%e shape_tid ~loc ~tname]]

let shape_top_app ~loc ~(tname:string) =
  [%expr Bin_prot.Shape.top_app _group [%e shape_tid ~loc ~tname]]

let shape_tuple ~loc (exps:expression list) =
  [%expr Bin_prot.Shape.tuple [%e elist ~loc exps]]

let shape_record ~loc (xs: (string * expression) list) =
  [%expr Bin_prot.Shape.record [%e elist ~loc (
    List.map xs ~f:(fun (s,e) ->
      [%expr ([%e estring ~loc s], [%e e])]))]]

let shape_variant ~loc (xs: (string * expression list) list) =
  [%expr Bin_prot.Shape.variant [%e elist ~loc (
    List.map xs ~f:(fun (s,es) ->
      [%expr ([%e estring ~loc s], [%e elist ~loc es])]))]]

let shape_poly_variant ~loc (xs: expression list) =
  [%expr Bin_prot.Shape.poly_variant [%e loc_string loc] [%e elist ~loc xs]]

type string_literal_or_other_expression =
  | String_literal of string
  | Other_expression of expression

let string_literal f s = f (String_literal s)
let other_expression f e = f (Other_expression e)

let shape_annotate ~loc ~name (x:expression) =
  let name =
    match name with
    | Other_expression e -> e
    | String_literal s ->
      [%expr Bin_prot.Shape.Uuid.of_string [%e estring ~loc s]]
  in
  [%expr Bin_prot.Shape.annotate [%e name] [%e x]]

let shape_basetype ~loc ~uuid (xs:expression list) =
  let uuid =
    match uuid with
    | Other_expression e -> e
    | String_literal s ->
      [%expr Bin_prot.Shape.Uuid.of_string [%e estring ~loc s]]
  in
  app_list ~loc [%expr Bin_prot.Shape.basetype [%e uuid]] xs

module Context : sig
  type t
  val create : type_declaration list -> t
  val is_local : t -> tname:string -> bool (* which names are defined in the local group *)
end = struct
  type t = { tds : type_declaration list }
  let create tds = { tds }
  let is_local t ~tname = List.exists t.tds ~f:(fun td -> String.equal tname td.ptype_name.txt)
end

let of_type : (
  allow_free_vars: bool ->
  context:Context.t -> core_type -> expression
) = fun ~allow_free_vars ~context ->

  let rec traverse_row ~loc ~typ_for_error (row : row_field) : expression =
    match row.prf_desc with
    | Rtag (_,true,_::_)
    | Rtag (_,false,_::_::_) ->
      raise_errorf ~loc "unsupported '&' in row_field: %s" (string_of_core_type typ_for_error)
    | Rtag ({ txt; _},true,[]) -> [%expr Bin_prot.Shape.constr [%e estring ~loc txt] None]
    | Rtag ({ txt; _},false,[t]) -> [%expr Bin_prot.Shape.constr [%e estring ~loc txt] (Some [%e traverse t])]
    | Rtag (_,false,[]) ->
      raise_errorf ~loc "impossible row_type: Rtag (_,_,false,[])"
    | Rinherit t ->
      [%expr Bin_prot.Shape.inherit_
               [%e loc_string { t.ptyp_loc with loc_ghost = true}] [%e traverse t]]

  and traverse typ =
    let loc = { typ.ptyp_loc with loc_ghost = true } in
    match typ.ptyp_desc with
    | Ptyp_constr (lid,typs) ->
      let args = List.map typs ~f:traverse in
      begin
        match
          match lid.txt with
          | Lident tname -> if Context.is_local context ~tname then Some tname else None
          | _ -> None
        with
        | Some tname -> app_list ~loc (shape_rec_app ~loc ~tname) args
        | None -> curry_app_list ~loc (bin_shape_lid ~loc lid) args
      end

    | Ptyp_tuple typs ->
      shape_tuple ~loc (List.map typs ~f:traverse)
    | Ptyp_var tvar ->
      if allow_free_vars
      then [%expr Bin_prot.Shape.var [%e loc_string loc] [%e shape_vid ~loc ~tvar]]
      else raise_errorf ~loc "unexpected free type variable: '%s" tvar

    | Ptyp_variant (rows,_,None) ->
      shape_poly_variant ~loc (List.map rows ~f:(fun row ->
        traverse_row ~loc ~typ_for_error:typ row))

    | Ptyp_poly (_,_)
    | Ptyp_variant (_,_,Some _)
    | Ptyp_any
    | Ptyp_arrow _
    | Ptyp_object _
    | Ptyp_class _
    | Ptyp_alias _
    | Ptyp_package _
    | Ptyp_extension _
      -> raise_errorf ~loc "unsupported type: %s" (string_of_core_type typ)
  in
  traverse

let tvars_of_def (td:type_declaration) : string list =
  List.map td.ptype_params ~f:(fun (typ,_variance) ->
    let loc = typ.ptyp_loc in
    match typ with
    | { ptyp_desc = Ptyp_var tvar; _ } -> tvar
    | _ -> raise_errorf ~loc "unexpected non-tvar in type params")

module Structure : sig

  val gen : (structure, rec_flag * type_declaration list) Deriving.Generator.t

end = struct

  let of_type = of_type ~allow_free_vars:true

  let of_label_decs ~loc ~context lds =
    shape_record ~loc (
      List.map lds ~f:(fun ld -> (ld.pld_name.txt, of_type ~context ld.pld_type)))

  let of_kind ~loc ~context (k:type_kind) : expression option =
    match k with
    | Ptype_record lds -> Some (of_label_decs ~loc ~context lds)
    | Ptype_variant cds ->
      Some (shape_variant ~loc (
        List.map cds ~f:(fun cd -> (
            cd.pcd_name.txt,
            begin match cd.pcd_args with
            | Pcstr_tuple args -> List.map args ~f:(of_type ~context)
            | Pcstr_record lds -> [of_label_decs ~loc ~context lds]
            end))))
    | Ptype_abstract ->
      None
    | Ptype_open ->
      raise_errorf ~loc "open types not supported"

  let expr_of_td ~loc ~context (td : type_declaration) : expression option =
    let expr =
      match of_kind ~loc ~context td.ptype_kind with
      | Some e -> Some e
      | None -> (* abstract type *)
        match td.ptype_manifest with
        | None ->
          (* A fully abstract type is usually intended to represent an empty type
             (0-constructor variant). *)
          Some (shape_variant ~loc [])
        | Some manifest -> Some (of_type ~context manifest)
    in
    expr

  let gen =
    Deriving.Generator.make Deriving.Args.(
      empty
      +> arg "annotate"
           ((map ~f:string_literal (estring __))
            ||| (map ~f:other_expression __))
      +> arg "annotate_provisionally"
           ((map ~f:string_literal (estring __))
            ||| (map ~f:other_expression __))
      +> arg "basetype"
           ((map ~f:string_literal (estring __))
            ||| (map ~f:other_expression __))
    ) (fun ~loc ~path:_ (rec_flag, tds)
        annotation_opt annotation_provisionally_opt basetype_opt ->
      let tds = List.map tds ~f:name_type_params_in_td in
      let context =
        match rec_flag with
        | Recursive -> Context.create tds
        | Nonrecursive -> Context.create []
      in
      let mk_pat mk_ =
        let pats = List.map tds ~f:(fun td ->
          let {Location.loc;txt=tname} = td.ptype_name in
          let name = mk_ tname in
          ppat_var ~loc (Loc.make name ~loc)
        )
        in
        ppat_tuple ~loc pats
      in
      let () =
        match annotation_provisionally_opt with
        | Some _ ->
          raise_errorf ~loc
            "[~annotate_provisionally] was renamed to [~annotate]. \
             Please use that."
        | None -> ()
      in
      let () =
        match annotation_opt,basetype_opt with
        | Some _,Some _ ->
          raise_errorf ~loc
            "cannot write both [bin_shape ~annotate] and [bin_shape ~basetype]"
        | _ -> ()
      in
      let () =
        match tds,annotation_opt with
        | ([] | _::_::_), Some _ ->
          raise_errorf ~loc
            "unexpected [~annotate] on multi type-declaration"
        | _ -> ()
      in
      let () =
        match tds,basetype_opt with
        | ([] | _::_::_), Some _ ->
          raise_errorf ~loc "unexpected [~basetype] on multi type-declaration"
        | _ -> ()
      in
      let annotate_f : (expression -> expression) =
        match annotation_opt with
        | None -> (fun e -> e)
        | Some name -> shape_annotate ~loc ~name
      in
      let tagged_schemes = List.filter_map tds ~f:(fun td ->
        let {Location.loc;txt=tname} = td.ptype_name in
        let body_opt  = expr_of_td ~loc ~context td in
        match body_opt with
        | None -> None
        | Some body ->
          let tvars = tvars_of_def td in
          let formals =
            List.map tvars ~f:(fun tvar -> shape_vid ~loc ~tvar)
          in
          [%expr ([%e shape_tid ~loc ~tname],
                  [%e elist ~loc formals],
                  [%e body])]
          |> fun x -> Some x
      )
      in
      let mk_exprs mk_init =
        let exprs =
          List.map tds ~f:(fun td ->
            let {Location.loc;txt=tname} = td.ptype_name in
            let tvars = tvars_of_def td in
            let args = List.map tvars ~f:(fun tvar -> evar ~loc tvar) in
            List.fold_right tvars
              ~init:(mk_init ~tname ~args)
              ~f:(fun tvar acc -> [%expr fun [%p pvar ~loc tvar] -> [%e acc]])
          )
        in
        [%expr [%e pexp_tuple ~loc exprs ] ]
      in
      let expr =
        match basetype_opt with
        | Some uuid ->
          mk_exprs (fun ~tname:_ ~args -> shape_basetype ~loc ~uuid args)
        | None ->
          [%expr
            let _group =
              Bin_prot.Shape.group [%e loc_string loc] [%e elist ~loc tagged_schemes]
            in
            [%e mk_exprs (fun ~tname ~args ->
              annotate_f (app_list ~loc (shape_top_app ~loc ~tname) args)
            )]]
      in
      let bindings = [value_binding ~loc ~pat:(mk_pat bin_shape_)  ~expr] in
      let structure = [
        pstr_value ~loc Nonrecursive bindings;
      ] in
      structure)

end

module Signature : sig

  val gen : (signature, rec_flag * type_declaration list) Deriving.Generator.t

end = struct

  let of_td td : signature_item =
    let td = name_type_params_in_td td in
    let {Location.loc;txt=tname} = td.ptype_name in
    let name = bin_shape_ tname in
    let tvars = tvars_of_def td in
    let type_ =
      List.fold_left tvars
        ~init: [%type: Bin_prot.Shape.t]
        ~f:(fun acc _ -> [%type: Bin_prot.Shape.t -> [%t acc]])
    in
    psig_value ~loc (value_description ~loc ~name:(Loc.make name ~loc) ~type_ ~prim:[])

  let gen =
    Deriving.Generator.make Deriving.Args.empty (fun ~loc:_ ~path:_ (_rec_flag, tds) ->
      List.map tds ~f:of_td
    )

end

let str_gen = Structure.gen
let sig_gen = Signature.gen

let shape_extension ~loc:_ typ =
  let context = Context.create [] in
  let allow_free_vars = false in
  of_type ~allow_free_vars ~context typ

let digest_extension ~loc typ =
  let loc = { loc with loc_ghost = true } in
  [%expr Bin_prot.Shape.Digest.to_hex (Bin_prot.Shape.eval_to_digest [%e shape_extension ~loc typ])]
