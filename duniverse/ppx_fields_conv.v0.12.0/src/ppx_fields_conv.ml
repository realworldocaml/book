(* Generated code should depend on the environment in scope as little as
   possible.  E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the
   use of [=].  It is especially important to not use polymorphic comparisons, since we
   are moving more and more to code that doesn't have them in scope. *)


open Base
open Printf
open Ppxlib
open Ast_builder.Default

module A = struct (* Additional AST construction helpers *)

  let exp_string : (loc:Location.t -> string -> expression) = fun ~loc s ->
    pexp_constant ~loc (Pconst_string (s,None))

  let pat_name : (loc:Location.t -> string -> pattern) = fun ~loc name ->
    ppat_var ~loc (Loc.make name ~loc)

  let exp_name : (loc:Location.t -> string -> expression) = fun ~loc name ->
    pexp_ident ~loc (Loc.make (Longident.Lident name) ~loc)

  let lid_name : (loc:Location.t -> string -> Longident.t loc) = fun ~loc name ->
    Loc.make (Longident.Lident name) ~loc

  let exp_true ~loc =
    pexp_construct ~loc (Located.mk ~loc (Longident.Lident "true")) None

  let str_item ~loc name body =
    pstr_value ~loc Nonrecursive [
      value_binding ~loc ~pat:(pat_name ~loc name) ~expr:body
    ]

  let mod_ ~loc : (string -> structure -> structure_item) =
    fun name structure ->
      pstr_module ~loc (module_binding ~loc ~name:(Located.mk ~loc name)
                          ~expr:(pmod_structure ~loc structure))

  let sig_item ~loc name typ =
    psig_value ~loc (
      value_description ~loc ~name:(Located.mk ~loc name) ~type_:typ ~prim:[]
    )

  let sig_mod ~loc : (string -> signature -> signature_item) =
    fun name signature ->
      psig_module ~loc (module_declaration ~loc ~name:(Located.mk ~loc name)
                          ~type_:(pmty_signature ~loc signature))

end

module Create = struct

  let record ~loc pairs =
    pexp_record ~loc
      (List.map pairs ~f:(fun (name,exp) -> A.lid_name ~loc name,exp))
      None

  let lambda ~loc patterns body =
    List.fold_right patterns ~init:body ~f:(fun (lab,pat) acc ->
      pexp_fun ~loc lab None pat acc)

  let lambda_sig ~loc arg_tys body_ty =
    List.fold_right arg_tys ~init:body_ty ~f:(fun (lab,arg_ty) acc ->
      ptyp_arrow ~loc lab arg_ty acc)

end

module Inspect = struct
  let field_names labdecs = List.map labdecs ~f:(fun labdec -> labdec.pld_name.txt)
end

let perm ~loc private_ =
  match private_ with
  | Private -> [%type:  [< `Read ] ]
  | Public -> [%type:  [< `Read | `Set_and_create ] ]

let field_t ~loc private_ tps =
  let id =
    match private_ with
    | Private -> Longident.parse "Fieldslib.Field.readonly_t"
    | Public -> Longident.parse "Fieldslib.Field.t"
  in
  ptyp_constr ~loc (Located.mk ~loc id) tps

let check_at_least_one_record ~loc rec_flag tds =
  (match rec_flag with
   | Nonrecursive ->
     Location.raise_errorf ~loc
       "nonrec is not compatible with the `fields' preprocessor"
   | _ -> ());
  let is_record td =
    match td.ptype_kind with
    | Ptype_record _ -> true
    | _ -> false
  in
  if not (List.exists tds ~f:is_record) then
    Location.raise_errorf ~loc
      (match tds with
       | [_] -> "Unsupported use of fields (you can only use it on records)."
       | _   -> "'with fields' can only be applied on type definitions in which at \
                 least one type definition is a record")
;;

module Gen_sig = struct

  let apply_type ~loc ~ty_name ~tps =
    ptyp_constr ~loc (A.lid_name ~loc ty_name) tps

  let label_arg name ty = (Labelled name,ty)

  let field_arg ~loc ~private_ ~record
      (f: field:core_type -> ty:core_type -> 'a) labdec : arg_label * 'a =
    let {pld_name=name; pld_type=ty; _} = labdec in
    label_arg name.txt (
      f ~field:(field_t ~loc private_ [record; ty]) ~ty)

  let create_fun ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let acc i = ptyp_var ~loc ("acc__" ^ Int.to_string i) in
    let f i = field_arg ~loc ~private_:Public ~record (fun ~field ~ty ->
      let create_f = [%type:  'input__ -> ( [%t ty] ) ] in
      [%type:  [%t field] -> [%t acc i] -> ([%t create_f] * [%t acc (i + 1)]) ]
    ) in
    let types = List.mapi labdecs ~f in
    let create_record_f = [%type:  'input__ -> ([%t record]) ] in
    let t = Create.lambda_sig ~loc
      (types @ [ Nolabel, acc 0 ])
      ([%type:  ( [%t create_record_f] * [%t acc (List.length labdecs)]) ])
    in
    A.sig_item ~loc "make_creator" t

  let simple_create_fun ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f labdec =
      let {pld_name=name; pld_type=ty; _} = labdec in
      label_arg name.txt ty
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc types record in
    A.sig_item ~loc "create" t

  let fold_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let acc i =
      ptyp_var ~loc ("acc__" ^ Int.to_string i)
    in
    let f i arg : (arg_label * core_type) =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:_ ->
        [%type:  [%t acc i] -> [%t field] -> [%t acc (i+1)]]) arg
    in
    let types = List.mapi labdecs ~f in
    let init_ty = label_arg "init" (acc 0) in
    let t = Create.lambda_sig ~loc
      (init_ty :: types)
      (acc (List.length labdecs)) in
    A.sig_item ~loc "fold" t

  let direct_fold_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let acc i =
      ptyp_var ~loc ("acc__" ^ Int.to_string i)
    in
    let f i arg =
      field_arg ~loc ~private_ ~record (fun ~field ~ty:field_ty ->
        [%type:  [%t acc i] -> [%t field] -> [%t record] -> [%t field_ty]
        -> [%t acc (i + 1)] ]) arg
    in
    let types = List.mapi labdecs ~f in
    let init_ty = label_arg "init" (acc 0) in
    let t =
      Create.lambda_sig ~loc ((Nolabel,record) :: init_ty :: types) (acc (List.length labdecs))
    in
      A.sig_item ~loc "fold" t

  let bool_fun fun_name ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f = field_arg ~loc ~private_ ~record (fun ~field ~ty:_ ->
      [%type:  [%t field] -> bool ] ) in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc types [%type:  bool ] in
    A.sig_item ~loc fun_name t

  let direct_bool_fun fun_name ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f = field_arg ~loc ~private_ ~record (fun ~field ~ty:field_ty ->
      [%type:  [%t field] -> [%t record] -> [%t field_ty] -> bool ] ) in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc ((Nolabel,record) :: types) [%type:  bool ] in
    A.sig_item ~loc fun_name t

  let iter_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f = field_arg ~loc ~private_ ~record (fun ~field ~ty:_ ->
      [%type:  [%t field] -> unit ]) in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc types [%type:  unit ] in
    A.sig_item ~loc "iter" t

  let direct_iter_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f = field_arg ~loc ~private_ ~record (fun ~field ~ty:field_ty ->
      [%type:  [%t field] -> [%t record] -> [%t field_ty] -> unit ]) in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc ((Nolabel,record) :: types) [%type:  unit ] in
    A.sig_item ~loc "iter" t

  let to_list_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f = field_arg ~loc ~private_ ~record (fun ~field ~ty:_ ->
      [%type:  [%t field] -> 'elem__ ])
    in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc types [%type:  'elem__ list] in
    A.sig_item ~loc "to_list" t

  let direct_to_list_fun ~private_ ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f = field_arg ~loc ~private_ ~record (fun ~field ~ty:field_ty ->
      [%type:  [%t field] -> [%t record] -> [%t field_ty] -> 'elem__ ]) in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc ((Nolabel,record) :: types) [%type:  'elem__ list] in
    A.sig_item ~loc "to_list" t

  let map_fun ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f = field_arg ~loc ~private_:Public ~record (fun ~field ~ty:field_ty ->
      [%type:  [%t field] -> [%t field_ty] ]) in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc types record in
    A.sig_item ~loc "map" t

  let direct_map_fun ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let f = field_arg ~loc ~private_:Public ~record (fun ~field ~ty:field_ty ->
      [%type:  [%t field] -> [%t record] -> [%t field_ty] -> [%t field_ty] ]) in
    let types = List.map labdecs ~f in
    let t = Create.lambda_sig ~loc ((Nolabel,record) :: types) record in
    A.sig_item ~loc "map" t

  let map_poly ~private_ ~ty_name ~tps ~loc _ =
    let record = apply_type ~loc ~ty_name ~tps in
    let tps_names =
      List.map tps ~f:(fun tp ->
        match tp.ptyp_desc with
        | Ptyp_var var -> var
        | _ -> assert false)
    in
    let fresh_variable =
      let rec loop i =
        let ret = sprintf "x%i" i in
        if List.mem ~equal:String.equal tps_names ret then
          loop (i+1)
        else
          ret
      in
      ptyp_var ~loc (loop 0)
    in
    let perm = perm ~loc private_ in
    let t =
      [%type:
        [%t ptyp_constr ~loc (Located.mk ~loc (Longident.parse "Fieldslib.Field.user"))
            [perm;record;fresh_variable]
        ]
        -> [%t fresh_variable] list
      ]
    in
    A.sig_item ~loc "map_poly" t

  let set_all_mutable_fields ~ty_name ~tps ~loc labdecs =
    let record = apply_type ~loc ~ty_name ~tps in
    let labels =
      List.fold_right labdecs ~init:[%type: unit] ~f:(fun labdec acc ->
        match labdec.pld_mutable with
        | Immutable -> acc
        | Mutable -> ptyp_arrow ~loc (Labelled labdec.pld_name.txt) labdec.pld_type acc
      )
    in
    A.sig_item ~loc "set_all_mutable_fields" [%type: [%t record] -> [%t labels]]

  let record ~private_ ~ty_name ~tps ~loc (labdecs:label_declaration list) : signature =
    let fields =
      List.rev_map labdecs ~f:(fun labdec ->
        let {pld_name={txt=name;loc}; pld_type=ty; _} = labdec in
        let record_ty = apply_type ~loc ~ty_name ~tps in
        let field = A.sig_item ~loc name (field_t ~loc private_ [record_ty; ty]) in
        field)
    in
    let getters_and_setters =
      List.concat (List.rev_map labdecs ~f:(fun labdec ->
        let {pld_name={txt=name;loc}; pld_type=ty; pld_mutable=m; _} = labdec in
        let record_ty = apply_type ~loc ~ty_name ~tps in
        let getter = A.sig_item ~loc name [%type: [%t record_ty] -> [%t ty] ] in
        match m, private_ with
        | Immutable, _
        | Mutable, Private -> [getter]
        | Mutable, Public ->
          let setter = A.sig_item ~loc ("set_" ^ name)
            [%type: [%t record_ty] -> [%t ty] -> unit]
          in
          [getter;setter]))
    in
    let create_fun = create_fun ~ty_name ~tps ~loc labdecs in
    let simple_create_fun = simple_create_fun ~ty_name ~tps ~loc labdecs in
    let fields_module = if String.equal ty_name "t" then "Fields" else "Fields_of_" ^ ty_name in
    let iter = iter_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let fold = fold_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let map = map_fun ~ty_name ~tps ~loc labdecs in
    let map_poly = map_poly ~private_ ~ty_name ~tps ~loc labdecs in
    let and_f = bool_fun "for_all" ~private_ ~ty_name ~tps ~loc labdecs in
    let or_f = bool_fun "exists" ~private_ ~ty_name ~tps ~loc labdecs in
    let to_list = to_list_fun ~private_ ~ty_name ~tps ~loc labdecs in

    let direct_iter = direct_iter_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let direct_fold = direct_fold_fun ~private_ ~ty_name ~tps ~loc labdecs in
    let direct_map = direct_map_fun ~ty_name ~tps ~loc labdecs in
    let direct_and_f = direct_bool_fun "for_all" ~private_ ~ty_name ~tps ~loc labdecs in
    let direct_or_f = direct_bool_fun "exists" ~private_ ~ty_name ~tps ~loc labdecs in
    let direct_to_list = direct_to_list_fun ~private_ ~ty_name ~tps ~loc labdecs in

    let set_all_mutable_fields = set_all_mutable_fields ~ty_name ~tps ~loc labdecs in
    getters_and_setters @ [
      A.sig_mod ~loc fields_module (
        List.concat [
          [A.sig_item ~loc "names" [%type: string list]];
          fields;
          [fold];
          begin match private_ with
          (* The ['perm] phantom type prohibits first-class fields from mutating or
             creating private records, so we can expose them (and fold, etc.).

             However, we still can't expose functions that explicitly create private
             records. *)
          | Private -> []
          | Public -> [
            create_fun;
            simple_create_fun;
            map;
          ]
          end;
          [iter; and_f; or_f; to_list; map_poly;
           A.sig_mod ~loc "Direct" (List.concat [
             [
               direct_iter;
               direct_fold;
               direct_and_f;
               direct_or_f;
               direct_to_list;
             ];
             begin match private_ with
            | Private -> []
            | Public -> [direct_map; set_all_mutable_fields]
            end;
           ])]])]

  let fields_of_td (td:type_declaration) : signature =
    let {ptype_name={txt=ty_name;loc};
         ptype_private=private_;
         ptype_params;
         ptype_kind; _ } = td in
    let tps = List.map ptype_params ~f:(fun (tp, _variance) -> tp) in
    match ptype_kind with
    | Ptype_record labdecs -> record ~private_ ~ty_name ~tps ~loc labdecs
    | _ -> []

  let generate ~loc ~path:_ (rec_flag, tds) =
    check_at_least_one_record ~loc rec_flag tds;
    List.concat_map tds ~f:fields_of_td

end

module Gen_struct = struct

  let gen_fields ~private_ ~loc (labdecs:label_declaration list) =
    let rec_id =
      match labdecs with
      | [] -> assert false
      | [_] -> None
      | _::_::_ -> Some [%expr _r__]
    in
    let conv_field labdec : (structure * structure_item) =
      let {pld_name={txt=name;loc}; pld_type=field_ty; pld_mutable=m; _} = labdec in
      let getter =
        A.str_item ~loc name
          [%expr fun _r__ -> [%e pexp_field ~loc [%expr _r__] (A.lid_name ~loc name)]]
      in
      let setter, setter_field =
        match m, private_ with
        | Mutable, Private ->
          [], [%expr Some (fun _ _ -> failwith "invalid call to a setter of a private type")]
        | Mutable, Public ->
            let setter =
              A.str_item ~loc ("set_" ^ name)
                [%expr fun _r__ v__ ->
                  [%e pexp_setfield ~loc
                      [%expr _r__] (A.lid_name ~loc name) [%expr v__] ]]
            in
            let setter_field = [%expr Some [%e A.exp_name ~loc ("set_" ^ name)]] in
            [setter], setter_field
        | Immutable, _ -> [], [%expr None ]
      in
      let field =
        let e = pexp_record ~loc [A.lid_name ~loc name, A.exp_name ~loc "v__"] rec_id in
        let fset =
          match private_ with
          | Private ->
            [%expr fun _ _ -> failwith "Invalid call to an fsetter of a private type" ]
          | Public -> [%expr fun _r__ v__ -> [%e e] ]
        in
        let perm = perm ~loc private_ in
        let annot =
          [%type: ([%t perm], _, [%t field_ty]) Fieldslib.Field.t_with_perm]
        in
        let body =
          [%expr
              Fieldslib.Field.Field { Fieldslib.Field.For_generated_code.
                force_variance = (fun (_ : [%t perm]) -> ());
                name = [%e A.exp_string ~loc name];
                getter = [%e A.exp_name ~loc name];
                setter = [%e setter_field];
                fset = [%e fset];}]
        in
        A.str_item ~loc name (pexp_constraint ~loc body annot)
      in
      getter::setter, field
    in
    let xss,ys = List.unzip (List.rev (List.map labdecs ~f:conv_field)) in
    List.concat xss, ys

  let label_arg ?label ~loc name =
    let l =
      match label with
      | None    -> name
      | Some n  -> n in
    (Labelled l),
    A.pat_name ~loc name

  let label_arg_fun ~loc name =
    label_arg ~label:name ~loc (name ^ "_fun__")

  let creation_fun ~loc _record_name labdecs =
    let names = Inspect.field_names labdecs  in
    let f =
      let body_record = Create.record ~loc
        (List.map names ~f:(fun n -> (n, A.exp_name ~loc n))) in
      let body =
        List.fold_right names
          ~init: [%expr ( [%e body_record] ) ]
          ~f: (fun field_name acc ->
            pexp_let ~loc Nonrecursive [
              value_binding ~loc ~pat:(A.pat_name ~loc field_name)
                ~expr:[%expr [%e A.exp_name ~loc (field_name ^ "_gen__")] acc__]
            ] acc)
      in
      Create.lambda ~loc [ (Nolabel,[%pat? acc__ ]) ] body
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let body0 = [%expr ([%e f], compile_acc__) ] in
    let body =
      List.fold_right names
        ~init: body0
        ~f: (fun field_name acc ->
          pexp_let ~loc Nonrecursive [
            value_binding ~loc
              ~pat:(ppat_tuple ~loc [
                (A.pat_name ~loc (field_name ^ "_gen__"));
                [%pat? compile_acc__];
              ])
              ~expr:[%expr
                [%e A.exp_name ~loc (field_name ^ "_fun__")]
                  [%e A.exp_name ~loc field_name]
                  compile_acc__]
          ] acc)
    in
    let f = Create.lambda ~loc (patterns @ [ (Nolabel,[%pat? compile_acc__ ]) ]) body in
    A.str_item ~loc "make_creator" f

  let simple_creation_fun ~loc _record_name labdecs =
    let names = Inspect.field_names labdecs  in
    let f = Create.record ~loc (List.map names ~f:(fun n -> (n, A.exp_name ~loc n))) in
    let patterns = List.map names ~f:(fun x -> label_arg ~loc x ) in
    let f = Create.lambda ~loc patterns f  in
    A.str_item ~loc "create" f

  let fold_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold acc_expr field_name =
      [%expr [%e A.exp_name ~loc (field_name ^ "_fun__") ]
          [%e acc_expr] [%e A.exp_name ~loc field_name] ]
    in
    let body = List.fold_left names ~init:[%expr init__ ] ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let init = label_arg ~label:"init" ~loc "init__" in
    let lambda = Create.lambda ~loc ( init :: patterns ) body in
    A.str_item ~loc "fold" lambda

  let direct_fold_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold acc_expr field_name =
      [%expr [%e A.exp_name ~loc (field_name ^ "_fun__") ]
        [%e acc_expr]
        [%e A.exp_name ~loc field_name]
        record__
        [%e pexp_field ~loc [%expr record__] (A.lid_name ~loc field_name)]
      ]
    in
    let body = List.fold_left names ~init:[%expr init__ ] ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let init = label_arg ~label:"init" ~loc "init__" in
    let lambda = Create.lambda ~loc ( (Nolabel,[%pat? record__ ]) :: init :: patterns ) body in
    A.str_item ~loc "fold" lambda

  let and_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold acc_expr field_name =
      [%expr [%e acc_expr] && [%e A.exp_name ~loc (field_name ^ "_fun__")]
          [%e A.exp_name ~loc field_name] ]
    in
    let body = List.fold_left names ~init:(A.exp_true ~loc) ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let lambda = Create.lambda ~loc patterns body in
    A.str_item ~loc "for_all" lambda

  let direct_and_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold acc_expr field_name =
      [%expr [%e acc_expr] && [%e A.exp_name ~loc (field_name ^ "_fun__") ]
        [%e A.exp_name ~loc field_name]
        record__
        [%e pexp_field ~loc [%expr record__] (A.lid_name ~loc field_name)]
      ]
    in
    let body = List.fold_left names ~init:(A.exp_true ~loc) ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let lambda = Create.lambda ~loc ((Nolabel,[%pat? record__ ]) :: patterns) body in
    A.str_item ~loc "for_all" lambda

  let or_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold acc_expr field_name =
      [%expr [%e acc_expr] || [%e A.exp_name ~loc (field_name ^ "_fun__")]
          [%e A.exp_name ~loc field_name] ] in
    let body =
      List.fold_left names ~init:[%expr false ] ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let lambda = Create.lambda ~loc patterns body in
    A.str_item ~loc "exists" lambda

  let direct_or_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let field_fold acc_expr field_name =
      [%expr [%e acc_expr] || [%e A.exp_name ~loc (field_name ^ "_fun__") ]
        [%e A.exp_name ~loc field_name]
        record__
        [%e pexp_field ~loc [%expr record__] (A.lid_name ~loc field_name)]
      ]
    in
    let body =
      List.fold_left names ~init:[%expr false ] ~f:field_fold in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let lambda = Create.lambda ~loc ((Nolabel,[%pat? record__ ]) :: patterns) body in
    A.str_item ~loc "exists" lambda

  let iter_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let iter_field field_name =
      [%expr
        ([%e A.exp_name ~loc (field_name ^ "_fun__") ]
           [%e A.exp_name ~loc field_name]
           : unit)
      ] in
    let body =
      List.fold_left (List.tl_exn names)
        ~init:(iter_field (List.hd_exn names))
        ~f:(fun acc n -> [%expr ( [%e acc] ; [%e iter_field n] ) ]) in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let lambda = Create.lambda ~loc (patterns) body in
    A.str_item ~loc "iter" lambda

  let direct_iter_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let iter_field field_name =
      [%expr [%e A.exp_name ~loc (field_name ^ "_fun__") ]
        [%e A.exp_name ~loc field_name]
        record__
        [%e pexp_field ~loc [%expr record__] (A.lid_name ~loc field_name)]
      ]
    in
    let body =
      List.fold_left (List.tl_exn names)
        ~init:(iter_field (List.hd_exn names))
        ~f:(fun acc n -> [%expr ( [%e acc] ; [%e iter_field n] ) ]) in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let lambda = Create.lambda ~loc ((Nolabel,[%pat? record__ ]) :: patterns) body in
    A.str_item ~loc "iter" lambda

  let map_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let body = Create.record ~loc (List.map names ~f:(fun field_name ->
      let e = [%expr [%e A.exp_name ~loc (field_name ^ "_fun__")]
                  [%e A.exp_name ~loc field_name] ]
      in (field_name, e )))
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let lambda = Create.lambda ~loc patterns body in
    A.str_item ~loc "map" lambda

  let direct_map_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let body = Create.record ~loc (List.map names ~f:(fun field_name ->
      let e = [%expr [%e A.exp_name ~loc (field_name ^ "_fun__")]
                  [%e A.exp_name ~loc field_name]
                  record__
                  [%e pexp_field ~loc [%expr record__] (A.lid_name ~loc field_name)]
              ]
      in (field_name, e )))
    in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let lambda = Create.lambda ~loc ((Nolabel,[%pat? record__ ]) :: patterns) body in
    A.str_item ~loc "map" lambda

  let to_list_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let fold field_name tail =
      [%expr [%e A.exp_name ~loc (field_name ^ "_fun__") ]
          [%e A.exp_name ~loc field_name] :: [%e tail] ]
    in
    let body = List.fold_right names ~init:[%expr [ ] ] ~f:fold in
    let lambda = Create.lambda ~loc patterns body in
    A.str_item ~loc "to_list" lambda

  let direct_to_list_fun ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let patterns = List.map names ~f:(label_arg_fun ~loc) in
    let fold field_name tail =
      [%expr [%e A.exp_name ~loc (field_name ^ "_fun__") ]
          [%e A.exp_name ~loc field_name]
          record__
          [%e pexp_field ~loc [%expr record__] (A.lid_name ~loc field_name)]
          :: [%e tail] ]
    in
    let body = List.fold_right names ~init:[%expr [ ] ] ~f:fold in
    let lambda = Create.lambda ~loc ((Nolabel,[%pat? record__ ]) :: patterns) body in
    A.str_item ~loc "to_list" lambda

  let map_poly ~loc labdecs =
    let names = Inspect.field_names labdecs in
    let fold name acc =
      [%expr record__.Fieldslib.Field.f [%e A.exp_name ~loc name] :: [%e acc] ]
    in
    let body = List.fold_right names ~init:[%expr []] ~f:fold in
    A.str_item ~loc "map_poly" (pexp_fun ~loc Nolabel None [%pat? record__] body)

  let sequence_ ~loc xs =
    match (List.rev xs) with
    | [] -> [%expr ()]
    | x::xs -> List.fold_left ~init:x xs ~f:(fun x y -> pexp_sequence ~loc y x)

  let set_all_mutable_fields ~loc labdecs =
    let record_name = "_record__" in
    let body =
      let exprs =
        List.fold_right labdecs ~init:[] ~f:(fun labdec acc ->
          match labdec.pld_mutable with
          | Immutable -> acc
          | Mutable ->
            let field_name = labdec.pld_name.txt in
            pexp_setfield ~loc
              (A.exp_name ~loc record_name)
              (A.lid_name ~loc field_name)
              (A.exp_name ~loc field_name)
            :: acc)
      in
      sequence_ ~loc exprs
    in
    let function_ =
      List.fold_right labdecs ~init:body ~f:(fun labdec acc ->
        match labdec.pld_mutable with
        | Immutable -> acc
        | Mutable ->
          let field_name = labdec.pld_name.txt in
          pexp_fun ~loc (Labelled field_name) None (A.pat_name ~loc field_name) acc)
    in
    A.str_item ~loc "set_all_mutable_fields" (
      pexp_fun ~loc Nolabel None (A.pat_name ~loc record_name) function_)

  let record ~private_ ~record_name ~loc (labdecs:label_declaration list) : structure =
    let getter_and_setters, fields = gen_fields ~private_ ~loc labdecs in
    let create = creation_fun ~loc record_name labdecs in
    let simple_create = simple_creation_fun ~loc record_name labdecs in
    let names = List.map (Inspect.field_names labdecs) ~f:(A.exp_string ~loc) in
    let fields_module =
      if String.equal record_name "t" then "Fields" else "Fields_of_" ^ record_name
    in
    let iter           = iter_fun ~loc labdecs in
    let fold           = fold_fun ~loc labdecs in
    let map            = map_fun ~loc labdecs in
    let map_poly       = map_poly ~loc labdecs in
    let andf           = and_fun ~loc labdecs in
    let orf            = or_fun ~loc labdecs in
    let to_list        = to_list_fun ~loc labdecs in
    let direct_iter    = direct_iter_fun ~loc labdecs in
    let direct_fold    = direct_fold_fun ~loc labdecs in
    let direct_andf    = direct_and_fun ~loc labdecs in
    let direct_orf     = direct_or_fun ~loc labdecs in
    let direct_map     = direct_map_fun ~loc labdecs in
    let direct_to_list = direct_to_list_fun ~loc labdecs in
    let set_all_mutable_fields = set_all_mutable_fields ~loc labdecs in
    getter_and_setters @ [
      A.mod_ ~loc fields_module (
        List.concat [
          [A.str_item ~loc "names" (elist ~loc names)];
          fields;
          begin match private_ with
          | Private -> []
          | Public -> [create; simple_create; map;]
          end;
          [iter; fold; map_poly;
           andf; orf; to_list;
          ];
          [A.mod_ ~loc "Direct" (List.concat [
            [
              direct_iter;
              direct_fold;
              direct_andf;
              direct_orf;
              direct_to_list;
            ];
            begin match private_ with
            | Private -> []
            | Public -> [direct_map; set_all_mutable_fields]
            end;
           ])]])]

  let fields_of_td (td:type_declaration) : structure =
    let {ptype_name={txt=record_name;loc};
         ptype_private=private_;
         ptype_kind; _ } = td in
    match ptype_kind with
    | Ptype_record labdecs -> record ~private_ ~record_name ~loc labdecs
    | _ -> []

  let generate ~loc ~path:_ (rec_flag, tds) =
    check_at_least_one_record ~loc rec_flag tds;
    List.concat_map tds ~f:fields_of_td

end

let fields =
  Deriving.add "fields"
    ~str_type_decl:(Deriving.Generator.make Deriving.Args.empty Gen_struct.generate)
    ~sig_type_decl:(Deriving.Generator.make Deriving.Args.empty Gen_sig.generate)
;;
