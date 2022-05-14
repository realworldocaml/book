open! Base
open! Ppxlib
open Ast_builder.Default
open Helpers
open Lifted.Monad_infix

(* Generates the signature for type conversion to S-expressions *)
module Sig_generate_sexp_of = struct
  let type_of_sexp_of ~loc t =
    let loc = { loc with loc_ghost = true } in
    [%type: [%t t] -> Sexplib0.Sexp.t]
  ;;

  let mk_type td = combinator_type_of_type_declaration td ~f:type_of_sexp_of

  let mk_sig ~loc:_ ~path:_ (_rf, tds) =
    List.map tds ~f:(fun td ->
      let loc = td.ptype_loc in
      psig_value
        ~loc
        (value_description
           ~loc
           ~name:(Located.map (( ^ ) "sexp_of_") td.ptype_name)
           ~type_:(mk_type td)
           ~prim:[]))
  ;;

  let mk_sig_exn ~loc:_ ~path:_ _te = []
end

module Str_generate_sexp_of = struct
  module Types_being_defined = struct
    type t =
      | Nonrec
      | Rec of Set.M(String).t

    let to_rec_flag = function
      | Nonrec -> Nonrecursive
      | Rec _ -> Recursive
    ;;
  end

  let sexp_of_type_constr ~loc id args =
    type_constr_conv ~loc id ~f:(fun s -> "sexp_of_" ^ s) args
  ;;

  (* Conversion of types *)
  let rec sexp_of_type ~renaming typ : Conversion.t =
    let loc = { typ.ptyp_loc with loc_ghost = true } in
    match typ with
    | _ when Option.is_some (Attribute.get Attrs.opaque typ) ->
      Conversion.of_reference_exn [%expr Sexplib0.Sexp_conv.sexp_of_opaque]
    | [%type: _] ->
      Conversion.of_lambda [ ppat_any ~loc --> [%expr Sexplib0.Sexp.Atom "_"] ]
    | [%type: [%t? _] sexp_opaque] ->
      Conversion.of_reference_exn [%expr Sexplib0.Sexp_conv.sexp_of_opaque]
    | { ptyp_desc = Ptyp_tuple tp; _ } ->
      Conversion.of_lambda [ sexp_of_tuple ~renaming (loc, tp) ]
    | { ptyp_desc = Ptyp_var parm; _ } ->
      (match Renaming.binding_kind renaming parm ~loc with
       | Universally_bound fresh ->
         Conversion.of_reference_exn (Fresh_name.expression fresh)
       | Existentially_bound -> sexp_of_type ~renaming [%type: _])
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      Conversion.of_reference_exn
        (sexp_of_type_constr
           ~loc
           id
           (List.map args ~f:(fun tp ->
              Conversion.to_expression ~loc (sexp_of_type ~renaming tp))))
    | { ptyp_desc = Ptyp_arrow (_, _, _); _ } ->
      Conversion.of_lambda
        [ ppat_any ~loc
          --> [%expr Sexplib0.Sexp_conv.sexp_of_fun Sexplib0.Sexp_conv.ignore]
        ]
    | { ptyp_desc = Ptyp_variant (row_fields, Closed, _); _ } ->
      sexp_of_variant ~renaming (loc, row_fields)
    | { ptyp_desc = Ptyp_poly (parms, poly_tp); _ } ->
      sexp_of_poly ~renaming parms poly_tp
    | { ptyp_desc = Ptyp_variant (_, Open, _); _ }
    | { ptyp_desc = Ptyp_object (_, _); _ }
    | { ptyp_desc = Ptyp_class (_, _); _ }
    | { ptyp_desc = Ptyp_alias (_, _); _ }
    | { ptyp_desc = Ptyp_package _; _ }
    | { ptyp_desc = Ptyp_extension _; _ } ->
      Location.raise_errorf ~loc "Type unsupported for ppx [sexp_of] conversion"

  (* Conversion of tuples *)
  and sexp_of_tuple ~renaming (loc, tps) =
    let fps = List.map ~f:(fun tp -> sexp_of_type ~renaming tp) tps in
    let ({ bindings; arguments; converted } : Conversion.Apply_all.t) =
      Conversion.apply_all ~loc fps
    in
    let in_expr = [%expr Sexplib0.Sexp.List [%e elist ~loc converted]] in
    let expr = pexp_let ~loc Nonrecursive bindings in_expr in
    ppat_tuple ~loc arguments --> expr

  (* Conversion of variant types *)
  and sexp_of_variant ~renaming ((loc, row_fields) : Location.t * row_field list)
    : Conversion.t
    =
    let item row =
      match row.prf_desc with
      | Rtag ({ txt = cnstr; _ }, true, []) ->
        ppat_variant ~loc cnstr None
        --> [%expr Sexplib0.Sexp.Atom [%e estring ~loc cnstr]]
      | Rtag ({ txt = cnstr; _ }, _, [ tp ])
        when Option.is_some (Attribute.get Attrs.list_poly row) ->
        (match tp with
         | [%type: [%t? tp] list] ->
           let cnv_expr = Conversion.to_expression ~loc (sexp_of_type ~renaming tp) in
           let name = Fresh_name.create "l" ~loc in
           ppat_variant ~loc cnstr (Some (Fresh_name.pattern name))
           --> [%expr
             Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom [%e estring ~loc cnstr]
                :: Sexplib0.Sexp_conv.list_map
                     [%e cnv_expr]
                     [%e Fresh_name.expression name])]
         | _ -> Attrs.invalid_attribute ~loc Attrs.list_poly "_ list")
      | Rtag ({ txt = cnstr; _ }, _, [ [%type: [%t? tp] sexp_list] ]) ->
        let cnv_expr = Conversion.to_expression ~loc (sexp_of_type ~renaming tp) in
        let name = Fresh_name.create "l" ~loc in
        ppat_variant ~loc cnstr (Some (Fresh_name.pattern name))
        --> [%expr
          Sexplib0.Sexp.List
            (Sexplib0.Sexp.Atom [%e estring ~loc cnstr]
             :: Sexplib0.Sexp_conv.list_map
                  [%e cnv_expr]
                  [%e Fresh_name.expression name])]
      | Rtag ({ txt = cnstr; _ }, false, [ tp ]) ->
        let cnstr_expr = [%expr Sexplib0.Sexp.Atom [%e estring ~loc cnstr]] in
        let fresh = Fresh_name.create "v" ~loc in
        let cnstr_arg =
          Conversion.apply ~loc (sexp_of_type ~renaming tp) (Fresh_name.expression fresh)
        in
        let expr = [%expr Sexplib0.Sexp.List [%e elist ~loc [ cnstr_expr; cnstr_arg ]]] in
        ppat_variant ~loc cnstr (Some (Fresh_name.pattern fresh)) --> expr
      | Rinherit { ptyp_desc = Ptyp_constr (id, []); _ } ->
        let name = Fresh_name.create "v" ~loc in
        ppat_alias ~loc (ppat_type ~loc id) (Fresh_name.to_string_loc name)
        --> sexp_of_type_constr ~loc id [ Fresh_name.expression name ]
      | Rtag (_, true, [ _ ]) | Rtag (_, _, _ :: _ :: _) ->
        Location.raise_errorf ~loc "unsupported: sexp_of_variant/Rtag/&"
      | Rinherit ({ ptyp_desc = Ptyp_constr (id, _ :: _); _ } as typ) ->
        let call = Conversion.to_expression ~loc (sexp_of_type ~renaming typ) in
        let name = Fresh_name.create "v" ~loc in
        ppat_alias ~loc (ppat_type ~loc id) (Fresh_name.to_string_loc name)
        --> [%expr [%e call] [%e Fresh_name.expression name]]
      | Rinherit _ ->
        Location.raise_errorf ~loc "unsupported: sexp_of_variant/Rinherit/non-id"
      (* impossible? *)
      | Rtag (_, false, []) -> assert false
    in
    Conversion.of_lambda (List.map ~f:item row_fields)

  (* Polymorphic record fields *)
  and sexp_of_poly ~renaming parms tp =
    let loc = tp.ptyp_loc in
    let renaming =
      List.fold_left
        parms
        ~init:renaming
        ~f:(Renaming.add_universally_bound ~prefix:"_of_")
    in
    let bindings =
      let mk_binding parm =
        let name =
          match Renaming.binding_kind renaming parm.txt ~loc:parm.loc with
          | Universally_bound name -> name
          | Existentially_bound -> assert false
        in
        value_binding
          ~loc
          ~pat:(Fresh_name.pattern name)
          ~expr:[%expr Sexplib0.Sexp_conv.sexp_of_opaque]
      in
      List.map ~f:mk_binding parms
    in
    Conversion.bind (sexp_of_type ~renaming tp) bindings
  ;;

  (* Conversion of record types *)

  let mk_rec_patt loc patt name fresh =
    let p = Loc.make (Longident.Lident name) ~loc, Fresh_name.pattern fresh in
    patt @ [ p ]
  ;;

  type is_empty_expr =
    | Inspect_value of (location -> expression -> expression)
    | Inspect_sexp of (cnv_expr:expression -> location -> expression -> expression)

  let sexp_of_record_field ~renaming ~bnds patt expr name tp ?sexp_of is_empty_expr =
    let loc = tp.ptyp_loc in
    let fresh = Fresh_name.create name ~loc in
    let patt = mk_rec_patt loc patt name fresh in
    let cnv_expr = Conversion.to_expression ~loc (sexp_of_type ~renaming tp) in
    let cnv_expr =
      match sexp_of with
      | None -> cnv_expr
      | Some sexp_of -> [%expr [%e sexp_of] [%e cnv_expr]]
    in
    let bnd = Fresh_name.create "bnd" ~loc in
    let arg = Fresh_name.create "arg" ~loc in
    let expr =
      [%expr
        let [%p Fresh_name.pattern bnds] =
          [%e
            match is_empty_expr with
            | Inspect_value is_empty_expr ->
              [%expr
                if [%e is_empty_expr loc (Fresh_name.expression fresh)]
                then [%e Fresh_name.expression bnds]
                else (
                  let [%p Fresh_name.pattern arg] =
                    [%e cnv_expr] [%e Fresh_name.expression fresh]
                  in
                  let [%p Fresh_name.pattern bnd] =
                    Sexplib0.Sexp.List
                      [ Sexplib0.Sexp.Atom [%e estring ~loc name]
                      ; [%e Fresh_name.expression arg]
                      ]
                  in
                  [%e Fresh_name.expression bnd] :: [%e Fresh_name.expression bnds])]
            | Inspect_sexp is_empty_expr ->
              [%expr
                let [%p Fresh_name.pattern arg] =
                  [%e cnv_expr] [%e Fresh_name.expression fresh]
                in
                if [%e is_empty_expr ~cnv_expr loc (Fresh_name.expression arg)]
                then [%e Fresh_name.expression bnds]
                else (
                  let [%p Fresh_name.pattern bnd] =
                    Sexplib0.Sexp.List
                      [ Sexplib0.Sexp.Atom [%e estring ~loc name]
                      ; [%e Fresh_name.expression arg]
                      ]
                  in
                  [%e Fresh_name.expression bnd] :: [%e Fresh_name.expression bnds])]]
        in
        [%e expr]]
    in
    patt, expr
  ;;

  let disallow_type_variables_and_recursive_occurrences
        ~types_being_defined
        ~loc
        ~attr_name
        tp
    =
    let disallow_variables =
      let iter =
        object
          inherit Ast_traverse.iter as super

          method! core_type_desc =
            function
            | Ptyp_var v ->
              Location.raise_errorf
                ~loc
                "[@%s] was used, but the type of the field contains a type variable: '%s.\n\
                 Comparison is not avaiable for type variables.\n\
                 Consider using [@sexp_drop_if _] or [@sexp_drop_default.sexp] instead."
                attr_name
                v
            | t -> super#core_type_desc t
        end
      in
      iter#core_type
    in
    let disallow_recursive_occurrences =
      match (types_being_defined : Types_being_defined.t) with
      | Nonrec -> fun _ -> ()
      | Rec types_being_defined ->
        let iter =
          object
            inherit Ast_traverse.iter as super

            method! core_type_desc =
              function
              | Ptyp_constr ({ loc = _; txt = Lident s }, _) as t ->
                if Set.mem types_being_defined s
                then
                  Location.raise_errorf
                    ~loc
                    "[@%s] was used, but the type of the field contains a type defined \
                     in the current recursive block: %s.\n\
                     This is not supported.\n\
                     Consider using [@sexp_drop_if _] or [@sexp_drop_default.sexp] \
                     instead."
                    attr_name
                    s;
                super#core_type_desc t
              | t -> super#core_type_desc t
          end
        in
        iter#core_type
    in
    disallow_variables tp;
    disallow_recursive_occurrences tp
  ;;

  let sexp_of_default_field
        ~types_being_defined
        how
        ~renaming
        ~bnds
        patt
        expr
        name
        tp
        ?sexp_of
        default
    =
    let is_empty =
      let inspect_value equality_f =
        Inspect_value (fun loc expr -> [%expr [%e equality_f loc] [%e default] [%e expr]])
      in
      match (how : Record_field_attrs.Sexp_of.Drop.t) with
      | Sexp ->
        Inspect_sexp
          (fun ~cnv_expr loc sexp_expr ->
             [%expr Sexplib0.Sexp_conv.( = ) ([%e cnv_expr] [%e default]) [%e sexp_expr]])
        |> Lifted.return
      | No_arg ->
        inspect_value (fun loc ->
          [%expr
            Sexplib0.Sexp_conv.( = ) [@ocaml.ppwarning
              "[@sexp_drop_default] is deprecated: please use \
               one of:\n\
               - [@sexp_drop_default f] and give an explicit \
               equality function ([f = Poly.(=)] corresponds \
               to the old behavior)\n\
               - [@sexp_drop_default.compare] if the type \
               supports [%compare]\n\
               - [@sexp_drop_default.equal] if the type \
               supports [%equal]\n\
               - [@sexp_drop_default.sexp] if you want to \
               compare the sexp representations\n"]])
        |> Lifted.return
      | Func lifted -> lifted >>| fun f -> inspect_value (fun _ -> f)
      | Compare ->
        inspect_value (fun loc ->
          disallow_type_variables_and_recursive_occurrences
            ~types_being_defined
            ~attr_name:"sexp_drop_default.compare"
            ~loc
            tp;
          [%expr [%compare.equal: [%t tp]]])
        |> Lifted.return
      | Equal ->
        inspect_value (fun loc ->
          disallow_type_variables_and_recursive_occurrences
            ~types_being_defined
            ~attr_name:"sexp_drop_default.equal"
            ~loc
            tp;
          [%expr [%equal: [%t tp]]])
        |> Lifted.return
    in
    is_empty >>| sexp_of_record_field ~renaming ~bnds patt expr name tp ?sexp_of
  ;;

  let sexp_of_label_declaration_list ~types_being_defined ~renaming loc flds ~wrap_expr =
    let bnds = Fresh_name.create "bnds" ~loc in
    let list_empty_expr =
      Inspect_value
        (fun loc lst ->
           [%expr
             match [%e lst] with
             | [] -> true
             | _ -> false])
    in
    let array_empty_expr =
      Inspect_value
        (fun loc arr ->
           [%expr
             match [%e arr] with
             | [||] -> true
             | _ -> false])
    in
    let coll lifted ld =
      lifted
      >>= fun ((patt : (Longident.t loc * pattern) list), expr) ->
      let name = ld.pld_name.txt in
      let loc = ld.pld_name.loc in
      let fresh = Fresh_name.create name ~loc in
      match Record_field_attrs.Sexp_of.create ~loc ld with
      | Sexp_option tp ->
        let v = Fresh_name.create "v" ~loc in
        let bnd = Fresh_name.create "bnd" ~loc in
        let arg = Fresh_name.create "arg" ~loc in
        let patt = mk_rec_patt loc patt name fresh in
        let vname = Fresh_name.expression v in
        let cnv_expr = Conversion.apply ~loc (sexp_of_type ~renaming tp) vname in
        let expr =
          [%expr
            let [%p Fresh_name.pattern bnds] =
              match [%e Fresh_name.expression fresh] with
              | Stdlib.Option.None -> [%e Fresh_name.expression bnds]
              | Stdlib.Option.Some [%p Fresh_name.pattern v] ->
                let [%p Fresh_name.pattern arg] = [%e cnv_expr] in
                let [%p Fresh_name.pattern bnd] =
                  Sexplib0.Sexp.List
                    [ Sexplib0.Sexp.Atom [%e estring ~loc name]
                    ; [%e Fresh_name.expression arg]
                    ]
                in
                [%e Fresh_name.expression bnd] :: [%e Fresh_name.expression bnds]
            in
            [%e expr]]
        in
        Lifted.return (patt, expr)
      | Sexp_bool ->
        let patt = mk_rec_patt loc patt name fresh in
        let bnd = Fresh_name.create "bnd" ~loc in
        let expr =
          [%expr
            let [%p Fresh_name.pattern bnds] =
              if [%e Fresh_name.expression fresh]
              then (
                let [%p Fresh_name.pattern bnd] =
                  Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom [%e estring ~loc name] ]
                in
                [%e Fresh_name.expression bnd] :: [%e Fresh_name.expression bnds])
              else [%e Fresh_name.expression bnds]
            in
            [%e expr]]
        in
        Lifted.return (patt, expr)
      | Sexp_list tp ->
        sexp_of_record_field
          ~renaming
          ~bnds
          patt
          expr
          name
          tp
          ~sexp_of:
            (* deliberately using whatever [sexp_of_list] is in scope *)
            [%expr sexp_of_list]
          list_empty_expr
        |> Lifted.return
      | Sexp_array tp ->
        sexp_of_record_field
          ~renaming
          ~bnds
          patt
          expr
          name
          tp
          ~sexp_of:
            (* deliberately using whatever [sexp_of_array] is in scope *)
            [%expr sexp_of_array]
          array_empty_expr
        |> Lifted.return
      | Specific (Drop_default how) ->
        let tp = ld.pld_type in
        (match Attribute.get Attrs.default ld with
         | None -> Location.raise_errorf ~loc "no default to drop"
         | Some { to_lift = default } ->
           Record_field_attrs.lift_default ~loc ld default
           >>= sexp_of_default_field
                 ~types_being_defined
                 how
                 ~renaming
                 ~bnds
                 patt
                 expr
                 name
                 tp)
      | Specific (Drop_if test) ->
        test
        >>| fun test ->
        let tp = ld.pld_type in
        sexp_of_record_field
          ~renaming
          ~bnds
          patt
          expr
          name
          tp
          (Inspect_value (fun loc expr -> [%expr [%e test] [%e expr]]))
      | Omit_nil ->
        let tp = ld.pld_type in
        let patt = mk_rec_patt loc patt name fresh in
        let vname = Fresh_name.expression fresh in
        let arg = Fresh_name.create "arg" ~loc in
        let cnv_expr = Conversion.apply ~loc (sexp_of_type ~renaming tp) vname in
        let bnds_expr =
          [%expr
            match [%e cnv_expr] with
            | Sexplib0.Sexp.List [] -> [%e Fresh_name.expression bnds]
            | [%p Fresh_name.pattern arg] ->
              Sexplib0.Sexp.List
                [ Sexplib0.Sexp.Atom [%e estring ~loc name]
                ; [%e Fresh_name.expression arg]
                ]
              :: [%e Fresh_name.expression bnds]]
        in
        ( patt
        , [%expr
          let [%p Fresh_name.pattern bnds] = [%e bnds_expr] in
          [%e expr]] )
        |> Lifted.return
      | Specific Keep ->
        let tp = ld.pld_type in
        let patt = mk_rec_patt loc patt name fresh in
        let vname = Fresh_name.expression fresh in
        let arg = Fresh_name.create "arg" ~loc in
        let cnv_expr = Conversion.apply ~loc (sexp_of_type ~renaming tp) vname in
        let bnds_expr =
          [%expr
            let [%p Fresh_name.pattern arg] = [%e cnv_expr] in
            Sexplib0.Sexp.List
              [ Sexplib0.Sexp.Atom [%e estring ~loc name]
              ; [%e Fresh_name.expression arg]
              ]
            :: [%e Fresh_name.expression bnds]]
        in
        ( patt
        , [%expr
          let [%p Fresh_name.pattern bnds] = [%e bnds_expr] in
          [%e expr]] )
        |> Lifted.return
    in
    let init_expr = wrap_expr (Fresh_name.expression bnds) in
    List.fold_left ~f:coll ~init:(Lifted.return ([], init_expr)) flds
    >>| fun (patt, expr) ->
    ( ppat_record ~loc patt Closed
    , [%expr
      let [%p Fresh_name.pattern bnds] = [] in
      [%e expr]] )
  ;;

  (* Conversion of sum types *)

  let branch_sum
        row
        inline_attr
        ~types_being_defined
        renaming
        ~loc
        constr_lid
        constr_str
        args
    =
    match args with
    | Pcstr_record lds ->
      let cnstr_expr = [%expr Sexplib0.Sexp.Atom [%e constr_str]] in
      sexp_of_label_declaration_list
        ~types_being_defined
        ~renaming
        loc
        lds
        ~wrap_expr:(fun expr -> [%expr Sexplib0.Sexp.List ([%e cnstr_expr] :: [%e expr])])
      >>| fun (patt, expr) -> ppat_construct ~loc constr_lid (Some patt) --> expr
    | Pcstr_tuple pcd_args ->
      (match pcd_args with
       | [] ->
         ppat_construct ~loc constr_lid None --> [%expr Sexplib0.Sexp.Atom [%e constr_str]]
         |> Lifted.return
       | args ->
         (match args with
          | [ tp ] when Option.is_some (Attribute.get inline_attr row) ->
            (match tp with
             | [%type: [%t? tp] list] ->
               let cnv_expr = Conversion.to_expression ~loc (sexp_of_type ~renaming tp) in
               let name = Fresh_name.create "l" ~loc in
               ppat_construct ~loc constr_lid (Some (Fresh_name.pattern name))
               --> [%expr
                 Sexplib0.Sexp.List
                   (Sexplib0.Sexp.Atom [%e constr_str]
                    :: Sexplib0.Sexp_conv.list_map
                         [%e cnv_expr]
                         [%e Fresh_name.expression name])]
             | _ -> Attrs.invalid_attribute ~loc inline_attr "_ list")
          | [ [%type: [%t? tp] sexp_list] ] ->
            let cnv_expr = Conversion.to_expression ~loc (sexp_of_type ~renaming tp) in
            let name = Fresh_name.create "l" ~loc in
            ppat_construct ~loc constr_lid (Some (Fresh_name.pattern name))
            --> [%expr
              Sexplib0.Sexp.List
                (Sexplib0.Sexp.Atom [%e constr_str]
                 :: Sexplib0.Sexp_conv.list_map
                      [%e cnv_expr]
                      [%e Fresh_name.expression name])]
          | _ ->
            let sexp_of_args = List.map ~f:(sexp_of_type ~renaming) args in
            let cnstr_expr = [%expr Sexplib0.Sexp.Atom [%e constr_str]] in
            let ({ bindings; arguments; converted } : Conversion.Apply_all.t) =
              Conversion.apply_all ~loc sexp_of_args
            in
            let patt =
              match arguments with
              | [ arg ] -> arg
              | _ -> ppat_tuple ~loc arguments
            in
            ppat_construct ~loc constr_lid (Some patt)
            --> pexp_let
                  ~loc
                  Nonrecursive
                  bindings
                  [%expr Sexplib0.Sexp.List [%e elist ~loc (cnstr_expr :: converted)]])
         |> Lifted.return)
  ;;

  let sexp_of_sum ~types_being_defined ~renaming tps cds =
    List.map cds ~f:(fun cd ->
      let renaming =
        Renaming.with_constructor_declaration renaming ~type_parameters:tps cd
      in
      let constr_lid = Located.map lident cd.pcd_name in
      let constr_str = estring ~loc:cd.pcd_name.loc cd.pcd_name.txt in
      branch_sum
        cd
        Attrs.list_variant
        ~types_being_defined
        renaming
        ~loc:cd.pcd_loc
        constr_lid
        constr_str
        cd.pcd_args)
    |> Lifted.all
    >>| Conversion.of_lambda
  ;;

  (* Empty type *)
  let sexp_of_nil loc = Conversion.of_lambda [ ppat_any ~loc --> [%expr assert false] ]

  (* Generate code from type definitions *)

  let sexp_of_td ~types_being_defined td =
    let td = name_type_params_in_td td in
    let tps = List.map td.ptype_params ~f:get_type_param_name in
    let { ptype_name = { txt = type_name; loc = _ }; ptype_loc = loc; _ } = td in
    let renaming = Renaming.of_type_declaration td ~prefix:"_of_" in
    let body =
      let body =
        match td.ptype_kind with
        | Ptype_variant cds ->
          sexp_of_sum
            ~renaming
            ~types_being_defined
            (List.map tps ~f:(fun x -> x.txt))
            cds
        | Ptype_record lds ->
          sexp_of_label_declaration_list
            ~renaming
            loc
            lds
            ~types_being_defined
            ~wrap_expr:(fun expr -> [%expr Sexplib0.Sexp.List [%e expr]])
          >>| fun (patt, expr) -> Conversion.of_lambda [ patt --> expr ]
        | Ptype_open ->
          Location.raise_errorf ~loc "ppx_sexp_conv: open types not supported"
        | Ptype_abstract ->
          (match td.ptype_manifest with
           | None -> sexp_of_nil loc
           | Some ty -> sexp_of_type ~renaming ty)
          |> Lifted.return
      in
      body
      >>| fun body ->
      let is_private_alias =
        match td.ptype_kind, td.ptype_manifest, td.ptype_private with
        | Ptype_abstract, Some _, Private -> true
        | _ -> false
      in
      if is_private_alias
      then (
        (* Replace all type variable by _ to avoid generalization problems *)
        let ty_src =
          core_type_of_type_declaration td |> replace_variables_by_underscores
        in
        let manifest =
          match td.ptype_manifest with
          | Some manifest -> manifest
          | None -> Location.raise_errorf ~loc "sexp_of_td/no-manifest"
        in
        let ty_dst = replace_variables_by_underscores manifest in
        let v = Fresh_name.create "v" ~loc in
        let coercion =
          [%expr ([%e Fresh_name.expression v] : [%t ty_src] :> [%t ty_dst])]
        in
        [%expr fun [%p Fresh_name.pattern v] -> [%e Conversion.apply ~loc body coercion]])
      else
        (* Prevent violation of value restriction, problems with recursive types, and
           top-level effects by eta-expanding function definitions *)
        Conversion.to_value_expression ~loc body
    in
    let typ = Sig_generate_sexp_of.mk_type td in
    let func_name = "sexp_of_" ^ type_name in
    let body =
      body
      >>| fun body ->
      let patts =
        List.map tps ~f:(fun id ->
          match Renaming.binding_kind renaming id.txt ~loc:id.loc with
          | Universally_bound name -> Fresh_name.pattern name
          | Existentially_bound -> assert false)
      in
      let rec_flag = Types_being_defined.to_rec_flag types_being_defined in
      eta_reduce_if_possible_and_nonrec ~rec_flag (eabstract ~loc patts body)
    in
    let body = Lifted.let_bind_user_expressions ~loc body in
    [ constrained_function_binding loc td typ ~tps ~func_name body ]
  ;;

  let sexp_of_tds ~loc ~path:_ (rec_flag, tds) =
    let rec_flag = really_recursive_respecting_opaque rec_flag tds in
    let (types_being_defined : Types_being_defined.t) =
      match rec_flag with
      | Nonrecursive -> Nonrec
      | Recursive ->
        Rec (Set.of_list (module String) (List.map tds ~f:(fun td -> td.ptype_name.txt)))
    in
    let bindings = List.concat_map tds ~f:(sexp_of_td ~types_being_defined) in
    pstr_value_list ~loc rec_flag bindings
  ;;

  let sexp_of_exn ~loc:_ ~path ec =
    let renaming = Renaming.without_type () in
    let get_full_cnstr str = path ^ "." ^ str in
    let loc = ec.ptyexn_loc in
    let expr =
      match ec.ptyexn_constructor with
      | { pext_name = cnstr; pext_kind = Pext_decl (_, extension_constructor_kind, None); _ }
        ->
        let constr_lid = Located.map lident cnstr in
        branch_sum
          ec
          Attrs.list_exception
          ~types_being_defined:Nonrec
          renaming
          ~loc
          constr_lid
          (estring ~loc (get_full_cnstr cnstr.txt))
          extension_constructor_kind
        >>| fun converter ->
        let assert_false = ppat_any ~loc --> [%expr assert false] in
        [%expr
          Sexplib0.Sexp_conv.Exn_converter.add
            [%extension_constructor [%e pexp_construct ~loc constr_lid None]]
            [%e
              Conversion.to_expression
                ~loc
                (Conversion.of_lambda [ converter; assert_false ])]]
      | { pext_kind = Pext_decl (_, _, Some _); _ } ->
        Location.raise_errorf ~loc "sexp_of_exn/:"
      | { pext_kind = Pext_rebind _; _ } ->
        Location.raise_errorf ~loc "sexp_of_exn/rebind"
    in
    let expr = Lifted.let_bind_user_expressions ~loc expr in
    [ pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:[%pat? ()] ~expr ] ]
  ;;

  let sexp_of_core_type core_type =
    let loc = { core_type.ptyp_loc with loc_ghost = true } in
    sexp_of_type ~renaming:(Renaming.without_type ()) core_type
    |> Conversion.to_value_expression ~loc
    |> Merlin_helpers.hide_expression
  ;;
end
