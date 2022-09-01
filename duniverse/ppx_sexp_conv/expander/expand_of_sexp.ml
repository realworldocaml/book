open! Base
open! Ppxlib
open Ast_builder.Default
open Helpers
open Lifted.Monad_infix

(* Generates the signature for type conversion from S-expressions *)
module Sig_generate_of_sexp = struct
  let type_of_of_sexp ~loc t =
    let loc = { loc with loc_ghost = true } in
    [%type: Sexplib0.Sexp.t -> [%t t]]
  ;;

  let mk_type td = combinator_type_of_type_declaration td ~f:type_of_of_sexp

  let sig_of_td with_poly td =
    let of_sexp_type = mk_type td in
    let loc = td.ptype_loc in
    let of_sexp_item =
      psig_value
        ~loc
        (value_description
           ~loc
           ~name:(Located.map (fun s -> s ^ "_of_sexp") td.ptype_name)
           ~type_:of_sexp_type
           ~prim:[])
    in
    match with_poly, is_polymorphic_variant td ~sig_:true with
    | true, `Surely_not ->
      Location.raise_errorf
        ~loc
        "Sig_generate_of_sexp.sig_of_td: sexp_poly annotation but type is surely not a \
         polymorphic variant"
    | false, (`Surely_not | `Maybe) -> [ of_sexp_item ]
    | (true | false), `Definitely | true, `Maybe ->
      [ of_sexp_item
      ; psig_value
          ~loc
          (value_description
             ~loc
             ~name:(Located.map (fun s -> "__" ^ s ^ "_of_sexp__") td.ptype_name)
             ~type_:of_sexp_type
             ~prim:[])
      ]
  ;;

  let mk_sig ~poly ~loc:_ ~path:_ (_rf, tds) = List.concat_map tds ~f:(sig_of_td poly)
end

module Str_generate_of_sexp = struct
  module Ptag_error_function = struct
    type t =
      | Ptag_no_args
      | Ptag_takes_args
  end

  module Row_or_constructor = struct
    type t =
      | Row of row_field
      | Constructor of constructor_declaration
  end

  let with_error_source ~loc ~full_type_name make_body =
    let lifted =
      let name = lazy (Fresh_name.create "error_source" ~loc) in
      make_body ~error_source:(fun () -> Fresh_name.expression (force name))
      >>| fun body ->
      match Lazy.is_val name with
      | false ->
        (* no references to [name], no need to define it *)
        body
      | true ->
        (* add a definition for [name] *)
        [%expr
          let [%p Fresh_name.pattern (force name)] = [%e estring ~loc full_type_name] in
          [%e body]]
    in
    Lifted.let_bind_user_expressions lifted ~loc
  ;;

  (* Utility functions for polymorphic variants *)

  (* Handle backtracking when variants do not match *)
  let handle_no_variant_match loc expr =
    [ [%pat? Sexplib0.Sexp_conv_error.No_variant_match] --> expr ]
  ;;

  (* Generate code depending on whether to generate a match for the last
     case of matching a variant *)
  let handle_variant_match_last loc ~match_last ~fresh_atom matches =
    match match_last, matches with
    | true, [ { pc_lhs = _; pc_guard = None; pc_rhs = expr } ]
    | _, [ { pc_lhs = [%pat? _]; pc_guard = None; pc_rhs = expr } ] -> expr
    | _ -> pexp_match ~loc (Fresh_name.expression fresh_atom) matches
  ;;

  (* Generate code for matching malformed S-expressions *)
  let mk_variant_other_matches ~error_source ~fresh__sexp loc rev_els call =
    let coll_structs acc (loc, cnstr) =
      (pstring ~loc cnstr
       -->
       match (call : Ptag_error_function.t) with
       | Ptag_no_args ->
         [%expr
           Sexplib0.Sexp_conv_error.ptag_no_args
             [%e error_source ()]
             [%e Fresh_name.expression fresh__sexp]]
       | Ptag_takes_args ->
         [%expr
           Sexplib0.Sexp_conv_error.ptag_takes_args
             [%e error_source ()]
             [%e Fresh_name.expression fresh__sexp]])
      :: acc
    in
    let exc_no_variant_match =
      [%pat? _] --> [%expr Sexplib0.Sexp_conv_error.no_variant_match ()]
    in
    List.fold_left ~f:coll_structs ~init:[ exc_no_variant_match ] rev_els
  ;;

  (* Split the row fields of a variant type into lists of atomic variants,
     structured variants, atomic variants + included variant types,
     and structured variants + included variant types. *)
  let split_row_field ~loc (atoms, structs, ainhs, sinhs) row_field =
    match row_field.prf_desc with
    | Rtag ({ txt = cnstr; _ }, true, []) ->
      let tpl = loc, cnstr in
      tpl :: atoms, structs, `A tpl :: ainhs, sinhs
    | Rtag ({ txt = cnstr; _ }, false, [ tp ]) ->
      let loc = tp.ptyp_loc in
      atoms, (loc, cnstr) :: structs, ainhs, `S (loc, cnstr, tp, row_field) :: sinhs
    | Rinherit inh ->
      let iinh = `I inh in
      atoms, structs, iinh :: ainhs, iinh :: sinhs
    | Rtag (_, true, [ _ ]) | Rtag (_, _, _ :: _ :: _) ->
      Location.raise_errorf ~loc "split_row_field/&"
    | Rtag (_, false, []) -> assert false
  ;;

  let type_constr_of_sexp ?(internal = false) id args =
    type_constr_conv id args ~f:(fun s ->
      let s = s ^ "_of_sexp" in
      if internal then "__" ^ s ^ "__" else s)
  ;;

  (* Conversion of types *)
  let rec type_of_sexp ~error_source ~typevars ?full_type ?(internal = false) typ
    : Conversion.t
    =
    let loc = typ.ptyp_loc in
    match typ with
    | _ when Option.is_some (Attribute.get Attrs.opaque typ) ->
      Conversion.of_reference_exn [%expr Sexplib0.Sexp_conv.opaque_of_sexp]
    | [%type: [%t? _] sexp_opaque] | [%type: _] ->
      Conversion.of_reference_exn [%expr Sexplib0.Sexp_conv.opaque_of_sexp]
    | [%type: [%t? ty1] sexp_list] ->
      let arg1 =
        Conversion.to_expression ~loc (type_of_sexp ~error_source ~typevars ty1)
      in
      Conversion.of_reference_exn [%expr Sexplib0.Sexp_conv.list_of_sexp [%e arg1]]
    | [%type: [%t? ty1] sexp_array] ->
      let arg1 =
        Conversion.to_expression ~loc (type_of_sexp ~error_source ~typevars ty1)
      in
      Conversion.of_reference_exn [%expr Sexplib0.Sexp_conv.array_of_sexp [%e arg1]]
    | { ptyp_desc = Ptyp_tuple tp; _ } ->
      Conversion.of_lambda (tuple_of_sexp ~error_source ~typevars (loc, tp))
    | { ptyp_desc = Ptyp_var parm; _ } ->
      (match Map.find typevars parm with
       | Some fresh -> Conversion.of_reference_exn (Fresh_name.expression fresh)
       | None -> Location.raise_errorf ~loc "ppx_sexp_conv: unbound type variable '%s" parm)
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      let args =
        List.map args ~f:(fun arg ->
          Conversion.to_expression ~loc (type_of_sexp ~error_source ~typevars arg))
      in
      Conversion.of_reference_exn (type_constr_of_sexp ~loc ~internal id args)
    | { ptyp_desc = Ptyp_arrow (_, _, _); _ } ->
      Conversion.of_reference_exn [%expr Sexplib0.Sexp_conv.fun_of_sexp]
    | { ptyp_desc = Ptyp_variant (row_fields, Closed, _); _ } ->
      variant_of_sexp ~error_source ~typevars ?full_type (loc, row_fields)
    | { ptyp_desc = Ptyp_poly (parms, poly_tp); _ } ->
      poly_of_sexp ~error_source ~typevars parms poly_tp
    | { ptyp_desc = Ptyp_variant (_, Open, _); _ }
    | { ptyp_desc = Ptyp_object (_, _); _ }
    | { ptyp_desc = Ptyp_class (_, _); _ }
    | { ptyp_desc = Ptyp_alias (_, _); _ }
    | { ptyp_desc = Ptyp_package _; _ }
    | { ptyp_desc = Ptyp_extension _; _ } ->
      Location.raise_errorf ~loc "Type unsupported for ppx [of_sexp] conversion"

  (* Conversion of tuples *)
  and tuple_of_sexp ~error_source ~typevars (loc, tps) =
    let fps = List.map ~f:(type_of_sexp ~error_source ~typevars) tps in
    let ({ bindings; arguments; converted } : Conversion.Apply_all.t) =
      Conversion.apply_all ~loc fps
    in
    let n = List.length fps in
    let fresh_sexp = Fresh_name.create "sexp" ~loc in
    [ [%pat? Sexplib0.Sexp.List [%p plist ~loc arguments]]
      --> pexp_let ~loc Nonrecursive bindings (pexp_tuple ~loc converted)
    ; Fresh_name.pattern fresh_sexp
      --> [%expr
        Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
          [%e error_source ()]
          [%e eint ~loc n]
          [%e Fresh_name.expression fresh_sexp]]
    ]

  (* Generate code for matching included variant types *)
  and handle_variant_inh
        ~error_source
        ~typevars
        ~fresh_atom
        ~fresh__sexp
        full_type
        ~match_last
        other_matches
        inh
    =
    let loc = inh.ptyp_loc in
    let func_expr = type_of_sexp ~error_source ~typevars ~internal:true inh in
    let app =
      Conversion.of_reference_exn
        (Conversion.apply ~loc func_expr (Fresh_name.expression fresh__sexp))
    in
    let match_exc =
      handle_no_variant_match
        loc
        (handle_variant_match_last loc ~match_last ~fresh_atom other_matches)
    in
    let new_other_matches =
      [ [%pat? _]
        --> pexp_try
              ~loc
              [%expr
                ([%e Conversion.to_expression ~loc app]
                 :> [%t replace_variables_by_underscores full_type])]
              match_exc
      ]
    in
    new_other_matches, true

  (* Generate code for matching atomic variants *)
  and mk_variant_match_atom
        ~error_source
        ~typevars
        ~fresh_atom
        ~fresh__sexp
        loc
        full_type
        rev_atoms_inhs
        rev_structs
    =
    let coll (other_matches, match_last) = function
      | `A (loc, cnstr) ->
        let new_match = pstring ~loc cnstr --> pexp_variant ~loc cnstr None in
        new_match :: other_matches, false
      | `I inh ->
        handle_variant_inh
          ~error_source
          ~typevars
          ~fresh_atom
          ~fresh__sexp
          full_type
          ~match_last
          other_matches
          inh
    in
    let other_matches =
      mk_variant_other_matches ~error_source ~fresh__sexp loc rev_structs Ptag_takes_args
    in
    let match_atoms_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_atoms_inhs
    in
    handle_variant_match_last loc ~match_last ~fresh_atom match_atoms_inhs

  (* Variant conversions *)

  (* Match arguments of constructors (variants or sum types) *)
  and mk_cnstr_args_match
        ~error_source
        ~typevars
        ~loc
        ~is_variant
        ~fresh__sexp
        ~fresh__tag
        ~fresh_sexp_args
        cnstr
        tps
        row
    =
    let cnstr vars_expr =
      if is_variant
      then pexp_variant ~loc cnstr (Some vars_expr)
      else pexp_construct ~loc (Located.lident ~loc cnstr) (Some vars_expr)
    in
    match tps with
    | [ tp ]
      when Option.is_some
             (match (row : Row_or_constructor.t) with
              | Row r -> Attribute.get Attrs.list_poly r
              | Constructor c -> Attribute.get Attrs.list_variant c) ->
      (match tp with
       | [%type: [%t? tp] list] ->
         let cnv =
           Conversion.to_expression ~loc (type_of_sexp ~error_source ~typevars tp)
         in
         cnstr
           [%expr
             Sexplib0.Sexp_conv.list_map
               [%e cnv]
               [%e Fresh_name.expression fresh_sexp_args]]
       | _ ->
         (match row with
          | Row _ -> Attrs.invalid_attribute ~loc Attrs.list_poly "_ list"
          | Constructor _ -> Attrs.invalid_attribute ~loc Attrs.list_variant "_ list"))
    | [ [%type: [%t? tp] sexp_list] ] ->
      let cnv = Conversion.to_expression ~loc (type_of_sexp ~error_source ~typevars tp) in
      cnstr
        [%expr
          Sexplib0.Sexp_conv.list_map [%e cnv] [%e Fresh_name.expression fresh_sexp_args]]
    | _ ->
      let bindings, patts, good_arg_match =
        let fps = List.map ~f:(type_of_sexp ~error_source ~typevars) tps in
        let ({ bindings; arguments; converted } : Conversion.Apply_all.t) =
          Conversion.apply_all ~loc fps
        in
        let good_arg_match = cnstr (pexp_tuple ~loc converted) in
        bindings, arguments, good_arg_match
      in
      [%expr
        match [%e Fresh_name.expression fresh_sexp_args] with
        | [%p plist ~loc patts] -> [%e pexp_let ~loc Nonrecursive bindings good_arg_match]
        | _ ->
          [%e
            if is_variant
            then
              [%expr
                Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                  [%e error_source ()]
                  [%e Fresh_name.expression fresh__tag]
                  [%e Fresh_name.expression fresh__sexp]]
            else
              [%expr
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  [%e error_source ()]
                  [%e Fresh_name.expression fresh__tag]
                  [%e Fresh_name.expression fresh__sexp]]]]

  (* Generate code for matching structured variants *)
  and mk_variant_match_struct
        ~error_source
        ~typevars
        ~fresh_atom
        ~fresh__sexp
        ~fresh_sexp_args
        loc
        full_type
        rev_structs_inhs
        rev_atoms
    =
    let has_structs_ref = ref false in
    let coll (other_matches, match_last) = function
      | `S (loc, cnstr, tp, row) ->
        has_structs_ref := true;
        let fresh__tag = Fresh_name.create "_tag" ~loc in
        let expr =
          mk_cnstr_args_match
            ~error_source
            ~typevars
            ~loc:tp.ptyp_loc
            ~is_variant:true
            ~fresh__sexp
            ~fresh__tag
            ~fresh_sexp_args
            cnstr
            [ tp ]
            (Row row)
        in
        let new_match =
          ppat_alias
            ~loc
            [%pat? [%p pstring ~loc cnstr]]
            (Fresh_name.to_string_loc fresh__tag)
          --> expr
        in
        new_match :: other_matches, false
      | `I inh ->
        handle_variant_inh
          ~error_source
          ~typevars
          ~fresh_atom
          ~fresh__sexp
          full_type
          ~match_last
          other_matches
          inh
    in
    let other_matches =
      mk_variant_other_matches ~error_source ~fresh__sexp loc rev_atoms Ptag_no_args
    in
    let match_structs_inhs, match_last =
      List.fold_left ~f:coll ~init:(other_matches, false) rev_structs_inhs
    in
    ( handle_variant_match_last loc ~match_last ~fresh_atom match_structs_inhs
    , !has_structs_ref )

  (* Generate code for handling atomic and structured variants (i.e. not
     included variant types) *)
  and handle_variant_tag ~error_source ~typevars loc full_type row_field_list =
    let fresh_atom = Fresh_name.create "atom" ~loc in
    let fresh_sexp = Fresh_name.create "sexp" ~loc in
    let fresh__sexp = Fresh_name.create "_sexp" ~loc in
    let fresh_sexp_args = Fresh_name.create "sexp_args" ~loc in
    let rev_atoms, rev_structs, rev_atoms_inhs, rev_structs_inhs =
      List.fold_left ~f:(split_row_field ~loc) ~init:([], [], [], []) row_field_list
    in
    let match_struct, has_structs =
      mk_variant_match_struct
        ~error_source
        ~typevars
        ~fresh_atom
        ~fresh__sexp
        ~fresh_sexp_args
        loc
        full_type
        rev_structs_inhs
        rev_atoms
    in
    let maybe_sexp_args_patt =
      if has_structs then Fresh_name.pattern fresh_sexp_args else [%pat? _]
    in
    [ ppat_alias
        ~loc
        [%pat? Sexplib0.Sexp.Atom [%p Fresh_name.pattern fresh_atom]]
        (Fresh_name.to_string_loc fresh__sexp)
      --> mk_variant_match_atom
            ~error_source
            ~typevars
            ~fresh_atom
            ~fresh__sexp
            loc
            full_type
            rev_atoms_inhs
            rev_structs
    ; ppat_alias
        ~loc
        [%pat?
               Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom [%p Fresh_name.pattern fresh_atom]
                :: [%p maybe_sexp_args_patt])]
        (Fresh_name.to_string_loc fresh__sexp)
      --> match_struct
    ; ppat_alias
        ~loc
        [%pat? Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _)]
        (Fresh_name.to_string_loc fresh_sexp)
      --> [%expr
        Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
          [%e error_source ()]
          [%e Fresh_name.expression fresh_sexp]]
    ; ppat_alias ~loc [%pat? Sexplib0.Sexp.List []] (Fresh_name.to_string_loc fresh_sexp)
      --> [%expr
        Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
          [%e error_source ()]
          [%e Fresh_name.expression fresh_sexp]]
    ]

  (* Generate matching code for variants *)
  and variant_of_sexp ~error_source ~typevars ?full_type (loc, row_fields) =
    let is_contained, full_type =
      match full_type with
      | None -> true, ptyp_variant ~loc row_fields Closed None
      | Some full_type -> false, full_type
    in
    let top_match =
      let fresh_sexp = Fresh_name.create ~loc "sexp" in
      match row_fields with
      | { prf_desc = Rinherit inh; _ } :: rest ->
        let rec loop inh row_fields =
          let call =
            [%expr
              ([%e
                Conversion.to_expression
                  ~loc
                  (type_of_sexp ~error_source ~typevars ~internal:true inh)]
                 [%e Fresh_name.expression fresh_sexp]
               :> [%t replace_variables_by_underscores full_type])]
          in
          match row_fields with
          | [] -> call
          | h :: t ->
            let expr =
              match h.prf_desc with
              | Rinherit inh -> loop inh t
              | _ ->
                let rftag_matches =
                  handle_variant_tag ~error_source ~typevars loc full_type row_fields
                in
                pexp_match ~loc (Fresh_name.expression fresh_sexp) rftag_matches
            in
            pexp_try ~loc call (handle_no_variant_match loc expr)
        in
        [ Fresh_name.pattern fresh_sexp --> loop inh rest ]
      | _ :: _ -> handle_variant_tag ~error_source ~typevars loc full_type row_fields
      | [] ->
        Location.raise_errorf
          ~loc
          "of_sexp is not supported for empty polymorphic variants (impossible?)"
    in
    if is_contained
    then (
      let fresh_sexp = Fresh_name.create "sexp" ~loc in
      Conversion.of_lambda
        [ Fresh_name.pattern fresh_sexp
          --> [%expr
            try [%e pexp_match ~loc (Fresh_name.expression fresh_sexp) top_match] with
            | Sexplib0.Sexp_conv_error.No_variant_match ->
              Sexplib0.Sexp_conv_error.no_matching_variant_found
                [%e error_source ()]
                [%e Fresh_name.expression fresh_sexp]]
        ])
    else Conversion.of_lambda top_match

  and poly_of_sexp ~error_source ~typevars parms tp =
    let loc = tp.ptyp_loc in
    let typevars =
      List.fold parms ~init:typevars ~f:(fun map parm ->
        Map.set
          map
          ~key:parm.txt
          ~data:(Fresh_name.create ("_of_" ^ parm.txt) ~loc:parm.loc))
    in
    let bindings =
      let mk_binding parm =
        let fresh = Map.find_exn typevars parm.txt in
        let fresh_sexp = Fresh_name.create "sexp" ~loc in
        value_binding
          ~loc
          ~pat:(Fresh_name.pattern fresh)
          ~expr:
            [%expr
              fun [%p Fresh_name.pattern fresh_sexp] ->
                Sexplib0.Sexp_conv_error.record_poly_field_value
                  [%e error_source ()]
                  [%e Fresh_name.expression fresh_sexp]]
      in
      List.map ~f:mk_binding parms
    in
    Conversion.bind (type_of_sexp ~error_source ~typevars tp) bindings
  ;;

  type field =
    { ld : label_declaration
    ; field_name : Fresh_name.t
    ; value_name : Fresh_name.t
    }

  let make_field ld =
    let field_name = Fresh_name.of_string_loc ld.pld_name in
    let value_name = Fresh_name.of_string_loc ld.pld_name in
    { ld; field_name; value_name }
  ;;

  (* Generate code for extracting record fields *)
  let mk_extract_fields
        ~error_source
        ~typevars
        ~allow_extra_fields
        ~fresh_duplicates
        ~fresh_extra
        ~fresh_field_name
        ~fresh_sexp
        ~fresh__field_sexp
        ~fresh__field_sexps
        (loc, flds)
    =
    let rec loop inits cases = function
      | [] -> inits, cases
      | fld :: more_flds ->
        let loc = fld.ld.pld_name.loc in
        let nm = fld.ld.pld_name.txt in
        (match Record_field_attrs.Of_sexp.create ~loc fld.ld, fld.ld.pld_type with
         | Sexp_bool, _ ->
           let inits = [%expr false] :: inits in
           let cases =
             (pstring ~loc nm
              --> [%expr
                if Stdlib.( ! ) [%e Fresh_name.expression fld.field_name]
                then
                  Stdlib.( := )
                    [%e Fresh_name.expression fresh_duplicates]
                    ([%e Fresh_name.expression fresh_field_name]
                     :: Stdlib.( ! ) [%e Fresh_name.expression fresh_duplicates])
                else (
                  match [%e Fresh_name.expression fresh__field_sexps] with
                  | [] -> Stdlib.( := ) [%e Fresh_name.expression fld.field_name] true
                  | _ :: _ ->
                    Sexplib0.Sexp_conv_error.record_sexp_bool_with_payload
                      [%e error_source ()]
                      [%e Fresh_name.expression fresh_sexp])])
             :: cases
           in
           loop inits cases more_flds
         | Sexp_option tp, _
         | ( ( Specific Required
             | Specific (Default _)
             | Omit_nil | Sexp_array _ | Sexp_list _ )
           , tp ) ->
           let inits = [%expr Stdlib.Option.None] :: inits in
           let unrolled =
             Conversion.apply
               ~loc
               (type_of_sexp ~error_source ~typevars tp)
               (Fresh_name.expression fresh__field_sexp)
           in
           let fresh_fvalue = Fresh_name.create "fvalue" ~loc in
           let cases =
             (pstring ~loc nm
              --> [%expr
                match Stdlib.( ! ) [%e Fresh_name.expression fld.field_name] with
                | Stdlib.Option.None ->
                  let [%p Fresh_name.pattern fresh__field_sexp] =
                    [%e Fresh_name.expression fresh__field_sexp] ()
                  in
                  let [%p Fresh_name.pattern fresh_fvalue] = [%e unrolled] in
                  Stdlib.( := )
                    [%e Fresh_name.expression fld.field_name]
                    (Stdlib.Option.Some [%e Fresh_name.expression fresh_fvalue])
                | Stdlib.Option.Some _ ->
                  Stdlib.( := )
                    [%e Fresh_name.expression fresh_duplicates]
                    ([%e Fresh_name.expression fresh_field_name]
                     :: Stdlib.( ! ) [%e Fresh_name.expression fresh_duplicates])])
             :: cases
           in
           loop inits cases more_flds)
    in
    let handle_extra =
      [ ([%pat? _]
         -->
         if allow_extra_fields
         then [%expr ()]
         else
           [%expr
             if Stdlib.( ! ) Sexplib0.Sexp_conv.record_check_extra_fields
             then
               Stdlib.( := )
                 [%e Fresh_name.expression fresh_extra]
                 ([%e Fresh_name.expression fresh_field_name]
                  :: Stdlib.( ! ) [%e Fresh_name.expression fresh_extra])
             else ()])
      ]
    in
    loop [] handle_extra (List.rev flds)
  ;;

  (* Generate code for handling the result of matching record fields *)
  let mk_handle_record_match_result
        ~error_source
        ~typevars
        ~fresh_sexp
        has_poly
        (loc, flds)
        ~wrap_expr
    =
    let has_nonopt_fields = ref false in
    let res_tpls, bi_lst, good_patts =
      let rec loop ((res_tpls, bi_lst, good_patts) as acc) = function
        | ({ ld = { pld_name = { txt = nm; loc }; _ }; _ } as fld) :: more_flds ->
          let fld_expr = [%expr Stdlib.( ! ) [%e Fresh_name.expression fld.field_name]] in
          let mk_default () = bi_lst, Fresh_name.pattern fld.value_name :: good_patts in
          let new_bi_lst, new_good_patts =
            match Record_field_attrs.Of_sexp.create ~loc fld.ld with
            | Specific (Default _)
            | Sexp_bool | Sexp_option _ | Sexp_list _ | Sexp_array _ | Omit_nil ->
              mk_default ()
            | Specific Required ->
              has_nonopt_fields := true;
              ( [%expr
                Sexplib0.Sexp_conv.( = ) [%e fld_expr] Stdlib.Option.None
              , [%e estring ~loc nm]]
                :: bi_lst
              , [%pat? Stdlib.Option.Some [%p Fresh_name.pattern fld.value_name]]
                :: good_patts )
          in
          let acc = fld_expr :: res_tpls, new_bi_lst, new_good_patts in
          loop acc more_flds
        | [] -> acc
      in
      loop ([], [], []) (List.rev flds)
    in
    let cnvt_value fld =
      match Record_field_attrs.Of_sexp.create ~loc fld.ld with
      | Sexp_list _ ->
        let fresh_v = Fresh_name.create "v" ~loc in
        [%expr
          match [%e Fresh_name.expression fld.value_name] with
          | Stdlib.Option.None -> []
          | Stdlib.Option.Some [%p Fresh_name.pattern fresh_v] ->
            [%e Fresh_name.expression fresh_v]]
        |> Lifted.return
      | Sexp_array _ ->
        let fresh_v = Fresh_name.create "v" ~loc in
        [%expr
          match [%e Fresh_name.expression fld.value_name] with
          | Stdlib.Option.None -> [||]
          | Stdlib.Option.Some [%p Fresh_name.pattern fresh_v] ->
            [%e Fresh_name.expression fresh_v]]
        |> Lifted.return
      | Specific (Default lifted_default) ->
        lifted_default
        >>= fun default ->
        let fresh_v = Fresh_name.create "v" ~loc in
        [%expr
          match [%e Fresh_name.expression fld.value_name] with
          | Stdlib.Option.None -> [%e default]
          | Stdlib.Option.Some [%p Fresh_name.pattern fresh_v] ->
            [%e Fresh_name.expression fresh_v]]
        |> Lifted.return
      | Sexp_bool | Sexp_option _ | Specific Required ->
        Fresh_name.expression fld.value_name |> Lifted.return
      | Omit_nil ->
        let fresh_e = Fresh_name.create "e" ~loc in
        let fresh_v = Fresh_name.create "v" ~loc in
        [%expr
          match [%e Fresh_name.expression fld.value_name] with
          | Stdlib.Option.Some [%p Fresh_name.pattern fresh_v] ->
            [%e Fresh_name.expression fresh_v]
          | Stdlib.Option.None ->
            (* We change the exception so it contains a sub-sexp of the
               initial sexp, otherwise sexplib won't find the source location
               for the error. *)
            (try
               [%e
                 Conversion.apply
                   ~loc
                   (type_of_sexp ~error_source ~typevars fld.ld.pld_type)
                   [%expr Sexplib0.Sexp.List []]]
             with
             | Sexplib0.Sexp_conv_error.Of_sexp_error ([%p Fresh_name.pattern fresh_e], _)
               ->
               Stdlib.raise
                 (Sexplib0.Sexp_conv_error.Of_sexp_error
                    ( [%e Fresh_name.expression fresh_e]
                    , [%e Fresh_name.expression fresh_sexp] )))]
        |> Lifted.return
    in
    let lifted_match_good_expr =
      if has_poly
      then List.map ~f:cnvt_value flds |> Lifted.all >>| pexp_tuple ~loc
      else (
        let cnvt fld =
          cnvt_value fld >>| fun field -> Located.lident ~loc fld.ld.pld_name.txt, field
        in
        List.map ~f:cnvt flds
        |> Lifted.all
        >>| fun fields -> wrap_expr (pexp_record ~loc fields None))
    in
    let expr = pexp_tuple ~loc res_tpls in
    let patt = ppat_tuple ~loc good_patts in
    lifted_match_good_expr
    >>| fun match_good_expr ->
    if !has_nonopt_fields
    then
      pexp_match
        ~loc
        expr
        [ patt --> match_good_expr
        ; [%pat? _]
          --> [%expr
            Sexplib0.Sexp_conv_error.record_undefined_elements
              [%e error_source ()]
              [%e Fresh_name.expression fresh_sexp]
              [%e elist ~loc bi_lst]]
        ]
    else pexp_match ~loc expr [ patt --> match_good_expr ]
  ;;

  (* Generate code for converting record fields *)
  let mk_cnv_fields
        ~error_source
        ~typevars
        ~allow_extra_fields
        ~fresh_sexp
        ~fresh_field_sexps
        has_poly
        (loc, flds)
        ~wrap_expr
    =
    let fresh_duplicates = Fresh_name.create ~loc "duplicates" in
    let fresh_extra = Fresh_name.create ~loc "extra" in
    let fresh_field_name = Fresh_name.create ~loc "field_name" in
    let fresh__field_sexp = Fresh_name.create ~loc "_field_sexp" in
    let fresh__field_sexps = Fresh_name.create ~loc "_field_sexps" in
    let expr_ref_inits, mc_fields =
      mk_extract_fields
        ~error_source
        ~typevars
        ~allow_extra_fields
        ~fresh_duplicates
        ~fresh_extra
        ~fresh_field_name
        ~fresh_sexp
        ~fresh__field_sexp
        ~fresh__field_sexps
        (loc, flds)
    in
    let field_refs =
      List.map2_exn flds expr_ref_inits ~f:(fun fld init ->
        value_binding
          ~loc
          ~pat:(Fresh_name.pattern fld.field_name)
          ~expr:[%expr Stdlib.ref [%e init]])
    in
    mk_handle_record_match_result
      ~error_source
      ~typevars
      ~fresh_sexp
      has_poly
      (loc, flds)
      ~wrap_expr
    >>| fun result_expr ->
    let fresh_iter = Fresh_name.create ~loc "iter" in
    let fresh_tail = Fresh_name.create ~loc "tail" in
    let fresh_x = Fresh_name.create ~loc "x" in
    pexp_let
      ~loc
      Nonrecursive
      (field_refs
       @ [ value_binding
             ~loc
             ~pat:(Fresh_name.pattern fresh_duplicates)
             ~expr:[%expr Stdlib.ref []]
         ; value_binding
             ~loc
             ~pat:(Fresh_name.pattern fresh_extra)
             ~expr:[%expr Stdlib.ref []]
         ])
      [%expr
        let rec [%p Fresh_name.pattern fresh_iter] =
          [%e
            pexp_function
              ~loc
              [ [%pat?
                       Sexplib0.Sexp.List
                       (Sexplib0.Sexp.Atom [%p Fresh_name.pattern fresh_field_name]
                        :: [%p
                          ppat_alias
                            ~loc
                            [%pat? [] | [ _ ]]
                            (Fresh_name.to_string_loc fresh__field_sexps)])
                     :: [%p Fresh_name.pattern fresh_tail]]
                --> [%expr
                  let [%p Fresh_name.pattern fresh__field_sexp] =
                    fun () ->
                      match [%e Fresh_name.expression fresh__field_sexps] with
                      | [ [%p Fresh_name.pattern fresh_x] ] ->
                        [%e Fresh_name.expression fresh_x]
                      | [] ->
                        Sexplib0.Sexp_conv_error.record_only_pairs_expected
                          [%e error_source ()]
                          [%e Fresh_name.expression fresh_sexp]
                      | _ -> assert false
                  in
                  [%e
                    pexp_match ~loc (Fresh_name.expression fresh_field_name) mc_fields];
                  [%e Fresh_name.expression fresh_iter]
                    [%e Fresh_name.expression fresh_tail]]
              ; [%pat?
                       [%p
                         ppat_alias
                           ~loc
                           [%pat? Sexplib0.Sexp.Atom _ | Sexplib0.Sexp.List _]
                           (Fresh_name.to_string_loc fresh_sexp)]
                     :: _]
                --> [%expr
                  Sexplib0.Sexp_conv_error.record_only_pairs_expected
                    [%e error_source ()]
                    [%e Fresh_name.expression fresh_sexp]]
              ; [%pat? []] --> [%expr ()]
              ]]
        in
        [%e Fresh_name.expression fresh_iter] [%e Fresh_name.expression fresh_field_sexps];
        match Stdlib.( ! ) [%e Fresh_name.expression fresh_duplicates] with
        | _ :: _ ->
          Sexplib0.Sexp_conv_error.record_duplicate_fields
            [%e error_source ()]
            (Stdlib.( ! ) [%e Fresh_name.expression fresh_duplicates])
            [%e Fresh_name.expression fresh_sexp]
        | [] ->
          (match Stdlib.( ! ) [%e Fresh_name.expression fresh_extra] with
           | _ :: _ ->
             Sexplib0.Sexp_conv_error.record_extra_fields
               [%e error_source ()]
               (Stdlib.( ! ) [%e Fresh_name.expression fresh_extra])
               [%e Fresh_name.expression fresh_sexp]
           | [] -> [%e result_expr])]
  ;;

  let is_poly (_, flds) =
    List.exists flds ~f:(function
      | { pld_type = { ptyp_desc = Ptyp_poly _; _ }; _ } -> true
      | _ -> false)
  ;;

  let label_declaration_list_of_sexp
        ~error_source
        ~typevars
        ~allow_extra_fields
        ~fresh_sexp
        ~fresh_field_sexps
        loc
        flds
        ~wrap_expr
    =
    let has_poly = is_poly (loc, flds) in
    let flds = List.map flds ~f:make_field in
    mk_cnv_fields
      ~error_source
      ~typevars
      ~allow_extra_fields
      ~fresh_sexp
      ~fresh_field_sexps
      has_poly
      (loc, flds)
      ~wrap_expr
    >>| fun cnv_fields ->
    if has_poly
    then (
      let flds =
        List.map flds ~f:(fun fld -> fld.ld, Fresh_name.of_string_loc fld.ld.pld_name)
      in
      let patt =
        ppat_tuple ~loc (List.map flds ~f:(fun (_, fresh) -> Fresh_name.pattern fresh))
      in
      let record_def =
        wrap_expr
          (pexp_record
             ~loc
             (List.map flds ~f:(fun ({ pld_name = { txt = name; loc }; _ }, fresh) ->
                Located.lident ~loc name, Fresh_name.expression fresh))
             None)
      in
      pexp_let
        ~loc
        Nonrecursive
        [ value_binding ~loc ~pat:patt ~expr:cnv_fields ]
        record_def)
    else cnv_fields
  ;;

  (* Generate matching code for records *)
  let record_of_sexp ~error_source ~typevars ~allow_extra_fields (loc, flds) =
    let fresh_sexp = Fresh_name.create "sexp" ~loc in
    let fresh_field_sexps = Fresh_name.create "field_sexps" ~loc in
    label_declaration_list_of_sexp
      ~error_source
      ~typevars
      ~allow_extra_fields
      ~fresh_sexp
      ~fresh_field_sexps
      loc
      flds
      ~wrap_expr:(fun x -> x)
    >>| fun success_expr ->
    Conversion.of_lambda
      [ ppat_alias
          ~loc
          [%pat? Sexplib0.Sexp.List [%p Fresh_name.pattern fresh_field_sexps]]
          (Fresh_name.to_string_loc fresh_sexp)
        --> success_expr
      ; ppat_alias ~loc [%pat? Sexplib0.Sexp.Atom _] (Fresh_name.to_string_loc fresh_sexp)
        --> [%expr
          Sexplib0.Sexp_conv_error.record_list_instead_atom
            [%e error_source ()]
            [%e Fresh_name.expression fresh_sexp]]
      ]
  ;;

  (* Sum type conversions *)

  (* Generate matching code for well-formed S-expressions wrt. sum types *)
  let mk_good_sum_matches ~error_source ~typevars (loc, cds) =
    List.map cds ~f:(fun cd ->
      match cd with
      | { pcd_name = cnstr; pcd_args = Pcstr_record fields; _ } ->
        let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
        let str = pstring ~loc cnstr.txt in
        let fresh_field_sexps = Fresh_name.create "field_sexps" ~loc in
        let fresh_sexp = Fresh_name.create "sexp" ~loc in
        let fresh__tag = Fresh_name.create "_tag" ~loc in
        label_declaration_list_of_sexp
          ~error_source
          ~typevars
          ~allow_extra_fields:
            (Option.is_some (Attribute.get Attrs.allow_extra_fields_cd cd))
          ~fresh_sexp
          ~fresh_field_sexps
          loc
          fields
          ~wrap_expr:(fun e ->
            pexp_construct ~loc (Located.lident ~loc cnstr.txt) (Some e))
        >>| fun expr ->
        ppat_alias
          ~loc
          [%pat?
                 Sexplib0.Sexp.List
                 (Sexplib0.Sexp.Atom
                    [%p
                      ppat_alias
                        ~loc
                        [%pat? [%p lcstr] | [%p str]]
                        (Fresh_name.to_string_loc fresh__tag)]
                  :: [%p Fresh_name.pattern fresh_field_sexps])]
          (Fresh_name.to_string_loc fresh_sexp)
        --> expr
      | { pcd_name = cnstr; pcd_args = Pcstr_tuple []; _ } ->
        Attrs.fail_if_allow_extra_field_cd ~loc cd;
        let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
        let str = pstring ~loc cnstr.txt in
        [%pat? Sexplib0.Sexp.Atom ([%p lcstr] | [%p str])]
        --> pexp_construct ~loc (Located.lident ~loc cnstr.txt) None
        |> Lifted.return
      | { pcd_name = cnstr; pcd_args = Pcstr_tuple (_ :: _ as tps); _ } ->
        Attrs.fail_if_allow_extra_field_cd ~loc cd;
        let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
        let str = pstring ~loc cnstr.txt in
        let fresh__sexp = Fresh_name.create "_sexp" ~loc in
        let fresh__tag = Fresh_name.create "_tag" ~loc in
        let fresh_sexp_args = Fresh_name.create "sexp_args" ~loc in
        ppat_alias
          ~loc
          [%pat?
                 Sexplib0.Sexp.List
                 (Sexplib0.Sexp.Atom
                    [%p
                      ppat_alias
                        ~loc
                        [%pat? [%p lcstr] | [%p str]]
                        (Fresh_name.to_string_loc fresh__tag)]
                  :: [%p Fresh_name.pattern fresh_sexp_args])]
          (Fresh_name.to_string_loc fresh__sexp)
        --> mk_cnstr_args_match
              ~error_source
              ~typevars
              ~loc
              ~is_variant:false
              ~fresh__sexp
              ~fresh__tag
              ~fresh_sexp_args
              cnstr.txt
              tps
              (Constructor cd)
        |> Lifted.return)
  ;;

  (* Generate matching code for malformed S-expressions with good tags
     wrt. sum types *)
  let mk_bad_sum_matches ~error_source (loc, cds) =
    let fresh_sexp = Fresh_name.create "sexp" ~loc in
    List.map cds ~f:(function
      | { pcd_name = cnstr; pcd_args = Pcstr_tuple []; _ } ->
        let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
        let str = pstring ~loc cnstr.txt in
        ppat_alias
          ~loc
          [%pat? Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ([%p lcstr] | [%p str]) :: _)]
          (Fresh_name.to_string_loc fresh_sexp)
        --> [%expr
          Sexplib0.Sexp_conv_error.stag_no_args
            [%e error_source ()]
            [%e Fresh_name.expression fresh_sexp]]
      | { pcd_name = cnstr; pcd_args = Pcstr_tuple (_ :: _) | Pcstr_record _; _ } ->
        let lcstr = pstring ~loc (String.uncapitalize cnstr.txt) in
        let str = pstring ~loc cnstr.txt in
        ppat_alias
          ~loc
          [%pat? Sexplib0.Sexp.Atom ([%p lcstr] | [%p str])]
          (Fresh_name.to_string_loc fresh_sexp)
        --> [%expr
          Sexplib0.Sexp_conv_error.stag_takes_args
            [%e error_source ()]
            [%e Fresh_name.expression fresh_sexp]])
  ;;

  (* Generate matching code for sum types *)
  let sum_of_sexp ~error_source ~typevars (loc, alts) =
    let fresh_sexp = Fresh_name.create "sexp" ~loc in
    [ mk_good_sum_matches ~error_source ~typevars (loc, alts) |> Lifted.all
    ; mk_bad_sum_matches ~error_source (loc, alts) |> Lifted.return
    ; [ ppat_alias
          ~loc
          [%pat? Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _)]
          (Fresh_name.to_string_loc fresh_sexp)
        --> [%expr
          Sexplib0.Sexp_conv_error.nested_list_invalid_sum
            [%e error_source ()]
            [%e Fresh_name.expression fresh_sexp]]
      ; ppat_alias
          ~loc
          [%pat? Sexplib0.Sexp.List []]
          (Fresh_name.to_string_loc fresh_sexp)
        --> [%expr
          Sexplib0.Sexp_conv_error.empty_list_invalid_sum
            [%e error_source ()]
            [%e Fresh_name.expression fresh_sexp]]
      ; Fresh_name.pattern fresh_sexp
        --> [%expr
          Sexplib0.Sexp_conv_error.unexpected_stag
            [%e error_source ()]
            [%e Fresh_name.expression fresh_sexp]]
      ]
      |> Lifted.return
    ]
    |> Lifted.all
    >>| List.concat
    >>| Conversion.of_lambda
  ;;

  (* Empty type *)
  let nil_of_sexp ~error_source loc : Conversion.t =
    Conversion.of_reference_exn
      [%expr Sexplib0.Sexp_conv_error.empty_type [%e error_source ()]]
  ;;

  (* Generate code from type definitions *)

  let td_of_sexp ~typevars ~loc:_ ~poly ~path ~rec_flag td =
    let tps = List.map td.ptype_params ~f:get_type_param_name in
    let { ptype_name = { txt = type_name; loc = _ }; ptype_loc = loc; _ } = td in
    let full_type =
      core_type_of_type_declaration td |> replace_variables_by_underscores
    in
    let is_private =
      match td.ptype_private with
      | Private -> true
      | Public -> false
    in
    if is_private
    then Location.raise_errorf ~loc "of_sexp is not supported for private type";
    let create_internal_function =
      match is_polymorphic_variant td ~sig_:false with
      | `Definitely -> true
      | `Maybe -> poly
      | `Surely_not ->
        if poly
        then
          Location.raise_errorf
            ~loc
            "sexp_poly annotation on a type that is surely not a polymorphic variant";
        false
    in
    let body ~error_source =
      let body =
        match td.ptype_kind with
        | Ptype_variant alts ->
          Attrs.fail_if_allow_extra_field_td ~loc td;
          sum_of_sexp ~error_source ~typevars (td.ptype_loc, alts)
        | Ptype_record lbls ->
          record_of_sexp
            ~error_source
            ~typevars
            ~allow_extra_fields:
              (Option.is_some (Attribute.get Attrs.allow_extra_fields_td td))
            (loc, lbls)
        | Ptype_open ->
          Location.raise_errorf ~loc "ppx_sexp_conv: open types not supported"
        | Ptype_abstract ->
          Attrs.fail_if_allow_extra_field_td ~loc td;
          (match td.ptype_manifest with
           | None -> nil_of_sexp ~error_source td.ptype_loc |> Lifted.return
           | Some ty ->
             type_of_sexp
               ~error_source
               ~full_type
               ~typevars
               ~internal:create_internal_function
               ty
             |> Lifted.return)
      in
      (* Prevent violation of value restriction, problems with recursive types, and
         toplevel effects by eta-expanding function definitions *)
      body >>| Conversion.to_value_expression ~loc
    in
    let external_name = type_name ^ "_of_sexp" in
    let internal_name = "__" ^ type_name ^ "_of_sexp__" in
    let arg_patts, arg_exprs =
      List.unzip
        (List.map
           ~f:(fun tp ->
             let name = Map.find_exn typevars tp.txt in
             Fresh_name.pattern name, Fresh_name.expression name)
           tps)
    in
    let full_type_name = Printf.sprintf "%s.%s" path type_name in
    let internal_fun_body =
      if create_internal_function
      then
        Some
          (with_error_source ~loc ~full_type_name (fun ~error_source ->
             body ~error_source
             >>| fun body ->
             eta_reduce_if_possible_and_nonrec ~rec_flag (eabstract ~loc arg_patts body)))
      else None
    in
    let external_fun_body =
      let body_below_lambdas ~error_source =
        let fresh_sexp = Fresh_name.create "sexp" ~loc in
        if create_internal_function
        then (
          let no_variant_match_mc =
            [ [%pat? Sexplib0.Sexp_conv_error.No_variant_match]
              --> [%expr
                Sexplib0.Sexp_conv_error.no_matching_variant_found
                  [%e error_source ()]
                  [%e Fresh_name.expression fresh_sexp]]
            ]
          in
          let internal_call =
            let internal_expr = pexp_ident ~loc { loc; txt = Lident internal_name } in
            eapply ~loc internal_expr (arg_exprs @ [ Fresh_name.expression fresh_sexp ])
          in
          let try_with = pexp_try ~loc internal_call no_variant_match_mc in
          [%expr fun [%p Fresh_name.pattern fresh_sexp] -> [%e try_with]] |> Lifted.return)
        else body ~error_source
      in
      let body_with_lambdas ~error_source =
        body_below_lambdas ~error_source
        >>| fun body ->
        eta_reduce_if_possible_and_nonrec ~rec_flag (eabstract ~loc arg_patts body)
      in
      with_error_source ~loc ~full_type_name body_with_lambdas
    in
    let typ = Sig_generate_of_sexp.mk_type td in
    let mk_binding func_name body =
      constrained_function_binding loc td typ ~tps ~func_name body
    in
    let internal_bindings =
      match internal_fun_body with
      | None -> []
      | Some body -> [ mk_binding internal_name body ]
    in
    let external_binding = mk_binding external_name external_fun_body in
    internal_bindings, [ external_binding ]
  ;;

  (* Generate code from type definitions *)
  let tds_of_sexp ~loc ~poly ~path (rec_flag, tds) =
    let tds = List.map ~f:name_type_params_in_td tds in
    let typevars td =
      List.fold
        td.ptype_params
        ~init:(Map.empty (module String))
        ~f:(fun map param ->
          let name = get_type_param_name param in
          Map.set
            map
            ~key:name.txt
            ~data:(Fresh_name.create ("_of_" ^ name.txt) ~loc:name.loc))
    in
    let singleton =
      match tds with
      | [ _ ] -> true
      | _ -> false
    in
    if singleton
    then (
      let rec_flag = really_recursive_respecting_opaque rec_flag tds in
      match rec_flag with
      | Recursive ->
        let bindings =
          List.concat_map tds ~f:(fun td ->
            let typevars = typevars td in
            let internals, externals =
              td_of_sexp ~typevars ~loc ~poly ~path ~rec_flag td
            in
            internals @ externals)
        in
        pstr_value_list ~loc Recursive bindings
      | Nonrecursive ->
        List.concat_map tds ~f:(fun td ->
          let typevars = typevars td in
          let internals, externals =
            td_of_sexp ~typevars ~loc ~poly ~path ~rec_flag td
          in
          pstr_value_list ~loc Nonrecursive internals
          @ pstr_value_list ~loc Nonrecursive externals))
    else (
      let bindings =
        List.concat_map tds ~f:(fun td ->
          let typevars = typevars td in
          let internals, externals =
            td_of_sexp ~typevars ~poly ~loc ~path ~rec_flag td
          in
          internals @ externals)
      in
      pstr_value_list ~loc rec_flag bindings)
  ;;

  let core_type_of_sexp ~path core_type =
    let loc = { core_type.ptyp_loc with loc_ghost = true } in
    let full_type_name =
      Printf.sprintf
        "%s line %i: %s"
        path
        loc.loc_start.pos_lnum
        (string_of_core_type core_type)
    in
    with_error_source ~loc ~full_type_name (fun ~error_source ->
      type_of_sexp ~error_source ~typevars:(Map.empty (module String)) core_type
      |> Conversion.to_value_expression ~loc
      |> Merlin_helpers.hide_expression
      |> Lifted.return)
  ;;
end
