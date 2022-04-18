open! Import

module Default = struct
  module Located = struct
    type 'a t = 'a Loc.t

    let loc (x : _ t) = x.loc
    let mk ~loc x = { loc; txt = x }
    let map f t = { t with txt = f t.txt }
    let map_lident x = map (fun x -> Longident.Lident x) x
    let lident ~loc x = mk ~loc (Longident.parse x)
  end

  include Ast_builder_generated.M

  let pstr_value_list ~loc rec_flag = function
    | [] -> []
    | vbs -> [ pstr_value ~loc rec_flag vbs ]

  let nonrec_type_declaration ~loc:_ ~name:_ ~params:_ ~cstrs:_ ~kind:_
      ~private_:_ ~manifest:_ =
    failwith
      "Ppxlib.Ast_builder.nonrec_type_declaration: don't use this function"

  let eint ~loc t = pexp_constant ~loc (Pconst_integer (Int.to_string t, None))
  let echar ~loc t = pexp_constant ~loc (Pconst_char t)
  let estring ~loc t = pexp_constant ~loc (Pconst_string (t, loc, None))
  let efloat ~loc t = pexp_constant ~loc (Pconst_float (t, None))

  let eint32 ~loc t =
    pexp_constant ~loc (Pconst_integer (Int32.to_string t, Some 'l'))

  let eint64 ~loc t =
    pexp_constant ~loc (Pconst_integer (Int64.to_string t, Some 'L'))

  let enativeint ~loc t =
    pexp_constant ~loc (Pconst_integer (Nativeint.to_string t, Some 'n'))

  let pint ~loc t = ppat_constant ~loc (Pconst_integer (Int.to_string t, None))
  let pchar ~loc t = ppat_constant ~loc (Pconst_char t)
  let pstring ~loc t = ppat_constant ~loc (Pconst_string (t, loc, None))
  let pfloat ~loc t = ppat_constant ~loc (Pconst_float (t, None))

  let pint32 ~loc t =
    ppat_constant ~loc (Pconst_integer (Int32.to_string t, Some 'l'))

  let pint64 ~loc t =
    ppat_constant ~loc (Pconst_integer (Int64.to_string t, Some 'L'))

  let pnativeint ~loc t =
    ppat_constant ~loc (Pconst_integer (Nativeint.to_string t, Some 'n'))

  let ebool ~loc t =
    pexp_construct ~loc (Located.lident ~loc (Bool.to_string t)) None

  let pbool ~loc t =
    ppat_construct ~loc (Located.lident ~loc (Bool.to_string t)) None

  let evar ~loc v = pexp_ident ~loc (Located.mk ~loc (Longident.parse v))
  let pvar ~loc v = ppat_var ~loc (Located.mk ~loc v)
  let eunit ~loc = pexp_construct ~loc (Located.lident ~loc "()") None
  let punit ~loc = ppat_construct ~loc (Located.lident ~loc "()") None
  let pexp_tuple ~loc l = match l with [ x ] -> x | _ -> pexp_tuple ~loc l
  let ppat_tuple ~loc l = match l with [ x ] -> x | _ -> ppat_tuple ~loc l
  let ptyp_tuple ~loc l = match l with [ x ] -> x | _ -> ptyp_tuple ~loc l

  let pexp_tuple_opt ~loc l =
    match l with [] -> None | _ :: _ -> Some (pexp_tuple ~loc l)

  let ppat_tuple_opt ~loc l =
    match l with [] -> None | _ :: _ -> Some (ppat_tuple ~loc l)

  let ptyp_poly ~loc vars ty =
    match vars with [] -> ty | _ -> ptyp_poly ~loc vars ty

  let pexp_apply ~loc e el =
    match (e, el) with
    | _, [] -> e
    | { pexp_desc = Pexp_apply (e, args); pexp_attributes = []; _ }, _ ->
        { e with pexp_desc = Pexp_apply (e, args @ el) }
    | _ -> pexp_apply ~loc e el

  let eapply ~loc e el =
    pexp_apply ~loc e (List.map el ~f:(fun e -> (Asttypes.Nolabel, e)))

  let eabstract ~loc ps e =
    List.fold_right ps ~init:e ~f:(fun p e ->
        pexp_fun ~loc Asttypes.Nolabel None p e)

  let esequence ~loc el =
    match el with
    | [] -> eunit ~loc
    | hd :: tl ->
        List.fold_left tl ~init:hd ~f:(fun acc e -> pexp_sequence ~loc acc e)

  let pconstruct cd arg =
    ppat_construct ~loc:cd.pcd_loc (Located.map_lident cd.pcd_name) arg

  let econstruct cd arg =
    pexp_construct ~loc:cd.pcd_loc (Located.map_lident cd.pcd_name) arg

  let rec elist ~loc l =
    match l with
    | [] -> pexp_construct ~loc (Located.mk ~loc (Longident.Lident "[]")) None
    | x :: l ->
        pexp_construct ~loc
          (Located.mk ~loc (Longident.Lident "::"))
          (Some (pexp_tuple ~loc [ x; elist ~loc l ]))

  let rec plist ~loc l =
    match l with
    | [] -> ppat_construct ~loc (Located.mk ~loc (Longident.Lident "[]")) None
    | x :: l ->
        ppat_construct ~loc
          (Located.mk ~loc (Longident.Lident "::"))
          (Some (ppat_tuple ~loc [ x; plist ~loc l ]))

  let unapplied_type_constr_conv_without_apply ~loc (ident : Longident.t) ~f =
    match ident with
    | Lident n -> pexp_ident ~loc { txt = Lident (f n); loc }
    | Ldot (path, n) -> pexp_ident ~loc { txt = Ldot (path, f n); loc }
    | Lapply _ ->
        Location.raise_errorf ~loc "unexpected applicative functor type"

  let type_constr_conv ~loc:apply_loc { Loc.loc; txt = longident } ~f args =
    let loc = { loc with loc_ghost = true } in
    match (longident : Longident.t) with
    | Lident _ | Ldot ((Lident _ | Ldot _), _) | Lapply _ -> (
        let ident =
          unapplied_type_constr_conv_without_apply longident ~loc ~f
        in
        match args with
        | [] -> ident
        | _ :: _ -> eapply ~loc:apply_loc ident args)
    | Ldot ((Lapply _ as module_path), n) ->
        let suffix_n functor_ = String.uncapitalize_ascii functor_ ^ "__" ^ n in
        let rec gather_lapply functor_args : Longident.t -> Longident.t * _ =
          function
          | Lapply (rest, arg) -> gather_lapply (arg :: functor_args) rest
          | Lident functor_ -> (Lident (suffix_n functor_), functor_args)
          | Ldot (functor_path, functor_) ->
              (Ldot (functor_path, suffix_n functor_), functor_args)
        in
        let ident, functor_args = gather_lapply [] module_path in
        eapply ~loc:apply_loc
          (unapplied_type_constr_conv_without_apply ident ~loc ~f)
          (List.map functor_args ~f:(fun path ->
               pexp_pack ~loc (pmod_ident ~loc { txt = path; loc }))
          @ args)

  let unapplied_type_constr_conv ~loc longident ~f =
    type_constr_conv longident ~loc ~f []

  let eta_reduce =
    let rec gather_params acc expr =
      match expr with
      | {
       pexp_desc =
         Pexp_fun (label, None (* no default expression *), subpat, body);
       pexp_attributes = [];
       pexp_loc = _;
       pexp_loc_stack = _;
      } -> (
          match subpat with
          | {
           ppat_desc = Ppat_var name;
           ppat_attributes = [];
           ppat_loc = _;
           ppat_loc_stack = _;
          } ->
              gather_params ((label, name, None) :: acc) body
          | {
           ppat_desc =
             Ppat_constraint
               ( {
                   ppat_desc = Ppat_var name;
                   ppat_attributes = [];
                   ppat_loc = _;
                   ppat_loc_stack = _;
                 },
                 ty );
           ppat_attributes = [];
           ppat_loc = _;
           ppat_loc_stack = _;
          } ->
              (* We reduce [fun (x : ty) -> f x] by rewriting it [(f : ty -> _)]. *)
              gather_params ((label, name, Some ty) :: acc) body
          | _ -> (List.rev acc, expr))
      | _ -> (List.rev acc, expr)
    in
    let annotate ~loc expr params =
      if List.exists params ~f:(fun (_, _, ty) -> Option.is_some ty) then
        let ty =
          List.fold_right params ~init:(ptyp_any ~loc)
            ~f:(fun (param_label, param, ty_opt) acc ->
              let loc = param.loc in
              let ty =
                match ty_opt with None -> ptyp_any ~loc | Some ty -> ty
              in
              ptyp_arrow ~loc param_label ty acc)
        in
        pexp_constraint ~loc expr ty
      else expr
    in
    let rec gather_args n x =
      if n = 0 then Some (x, [])
      else
        match x with
        | {
         pexp_desc = Pexp_apply (body, args);
         pexp_attributes = [];
         pexp_loc = _;
         pexp_loc_stack = _;
        } ->
            if List.length args <= n then
              match gather_args (n - List.length args) body with
              | None -> None
              | Some (body, args') -> Some (body, args' @ args)
            else None
        | _ -> None
    in
    fun expr ->
      let params, body = gather_params [] expr in
      match gather_args (List.length params) body with
      | None -> None
      | Some (({ pexp_desc = Pexp_ident _; _ } as f_ident), args) -> (
          match
            List.for_all2 args params
              ~f:(fun (arg_label, arg) (param_label, param, _) ->
                Poly.( = ) (arg_label : arg_label) param_label
                &&
                match arg with
                | {
                 pexp_desc = Pexp_ident { txt = Lident name'; _ };
                 pexp_attributes = [];
                 pexp_loc = _;
                 pexp_loc_stack = _;
                } ->
                    String.( = ) name' param.txt
                | _ -> false)
          with
          | false -> None
          | true -> Some (annotate ~loc:expr.pexp_loc f_ident params))
      | _ -> None

  let eta_reduce_if_possible expr = Option.value (eta_reduce expr) ~default:expr

  let eta_reduce_if_possible_and_nonrec expr ~rec_flag =
    match rec_flag with
    | Recursive -> expr
    | Nonrecursive -> eta_reduce_if_possible expr
end

module type Loc = Ast_builder_intf.Loc
module type S = Ast_builder_intf.S

module Make (Loc : sig
  val loc : Location.t
end) : S = struct
  include Ast_builder_generated.Make (Loc)

  let pstr_value_list = Default.pstr_value_list

  let nonrec_type_declaration ~name ~params ~cstrs ~kind ~private_ ~manifest =
    Default.nonrec_type_declaration ~loc ~name ~params ~cstrs ~kind ~private_
      ~manifest

  module Located = struct
    include Default.Located

    let loc _ = Loc.loc
    let mk x = mk ~loc:Loc.loc x
    let lident x = lident ~loc:Loc.loc x
  end

  let pexp_tuple l = Default.pexp_tuple ~loc l
  let ppat_tuple l = Default.ppat_tuple ~loc l
  let ptyp_tuple l = Default.ptyp_tuple ~loc l
  let pexp_tuple_opt l = Default.pexp_tuple_opt ~loc l
  let ppat_tuple_opt l = Default.ppat_tuple_opt ~loc l
  let ptyp_poly vars ty = Default.ptyp_poly ~loc vars ty
  let pexp_apply e el = Default.pexp_apply ~loc e el
  let eint t = Default.eint ~loc t
  let echar t = Default.echar ~loc t
  let estring t = Default.estring ~loc t
  let efloat t = Default.efloat ~loc t
  let eint32 t = Default.eint32 ~loc t
  let eint64 t = Default.eint64 ~loc t
  let enativeint t = Default.enativeint ~loc t
  let ebool t = Default.ebool ~loc t
  let evar t = Default.evar ~loc t
  let pint t = Default.pint ~loc t
  let pchar t = Default.pchar ~loc t
  let pstring t = Default.pstring ~loc t
  let pfloat t = Default.pfloat ~loc t
  let pint32 t = Default.pint32 ~loc t
  let pint64 t = Default.pint64 ~loc t
  let pnativeint t = Default.pnativeint ~loc t
  let pbool t = Default.pbool ~loc t
  let pvar t = Default.pvar ~loc t
  let eunit = Default.eunit ~loc
  let punit = Default.punit ~loc
  let econstruct = Default.econstruct
  let pconstruct = Default.pconstruct
  let eapply e el = Default.eapply ~loc e el
  let eabstract ps e = Default.eabstract ~loc ps e
  let esequence el = Default.esequence ~loc el
  let elist l = Default.elist ~loc l
  let plist l = Default.plist ~loc l

  let type_constr_conv ident ~f args =
    Default.type_constr_conv ~loc ident ~f args

  let unapplied_type_constr_conv ident ~f =
    Default.unapplied_type_constr_conv ~loc ident ~f

  let eta_reduce = Default.eta_reduce
  let eta_reduce_if_possible = Default.eta_reduce_if_possible

  let eta_reduce_if_possible_and_nonrec =
    Default.eta_reduce_if_possible_and_nonrec
end

let make loc =
  (module Make (struct
    let loc = loc
  end) : S)
