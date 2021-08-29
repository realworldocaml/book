open Stdppx
open Ppxlib
open Ast_builder.Default

let alphabet =
  Array.init (Char.code 'z' - Char.code 'a' + 1)
    ~f:(fun i -> String.make 1 (Char.chr (i + Char.code 'a')))
;;

let vars_of_list ~get_loc l =
  List.mapi l ~f:(fun i x -> { txt = alphabet.(i); loc = get_loc x })

let evar_of_var { txt; loc } = evar ~loc txt
let pvar_of_var { txt; loc } = pvar ~loc txt
let tvar_of_var { txt; loc } = ptyp_var ~loc txt

let evars_of_vars = List.map ~f:evar_of_var
let pvars_of_vars = List.map ~f:pvar_of_var
let tvars_of_vars = List.map ~f:tvar_of_var

module Backends = struct
  class reconstructors = object
    method record ~loc flds = pexp_record ~loc flds None
    method construct ~loc id args =
      pexp_construct ~loc id
        (match args with
         | [] -> None
         | _  -> Some (pexp_tuple ~loc args))
    method tuple ~loc es = pexp_tuple ~loc es
  end

  class type what = object
    method name : string

    inherit reconstructors

    method class_params : loc:Location.t -> (core_type * (variance * injectivity)) list

    method apply
      :  loc:Location.t
      -> expression
      -> expression list
      -> expression

    method abstract
      :  loc:Location.t
      -> pattern
      -> expression
      -> expression

    (* Basic combinator type *)
    method typ : loc:Location.t -> core_type -> core_type

    method any : loc:Location.t -> expression

    method combine
      :  loc:Location.t
      -> (string loc * expression) list
      -> reconstruct:expression
      -> expression
  end

  let mapper : what = object
    method name = "map"

    inherit reconstructors

    method class_params ~loc:_ = []

    method apply ~loc expr args = eapply ~loc expr args
    method abstract ~loc patt expr = pexp_fun ~loc Nolabel None patt expr

    method typ ~loc ty = ptyp_arrow ~loc Nolabel ty ty

    method any ~loc = [%expr fun x -> x]

    method combine ~loc combinators ~reconstruct =
      List.fold_right combinators ~init:reconstruct ~f:(fun (v, expr) acc ->
        pexp_let ~loc Nonrecursive [value_binding ~loc ~pat:(pvar_of_var v) ~expr] acc)
  end

  let iterator : what = object
    method name = "iter"

    inherit reconstructors

    method class_params ~loc:_ = []

    method apply ~loc expr args = eapply ~loc expr args
    method abstract ~loc patt expr = pexp_fun ~loc Nolabel None patt expr

    method typ ~loc ty = [%type: [%t ty] -> unit]

    method any ~loc = [%expr fun _ -> ()]

    method combine ~loc combinators ~reconstruct:_ =
      match List.rev combinators with
      | [] -> [%expr ()]
      | (_, expr) :: rest ->
        List.fold_left rest ~init:expr ~f:(fun acc (_v, expr) ->
          pexp_sequence ~loc expr acc)
  end

  let folder : what = object
    method name = "fold"

    inherit reconstructors

    method class_params ~loc = [(ptyp_var ~loc "acc", (NoVariance, NoInjectivity))]

    method apply ~loc expr args = eapply ~loc expr (args @ [evar ~loc "acc"])
    method abstract ~loc patt expr =
      eabstract ~loc [patt; pvar ~loc "acc"] expr

    method typ ~loc ty = [%type: [%t ty] -> 'acc -> 'acc]

    method any ~loc = [%expr fun _ acc -> acc]

    method combine ~loc combinators ~reconstruct:_ =
      match combinators with
      | [(_, expr)] -> expr
      | _ ->
        List.fold_right combinators ~init:[%expr acc] ~f:(fun (_v, expr) acc ->
          [%expr
            let acc = [%e expr] in
            [%e acc]
          ])
  end

  let fold_mapper : what = object
    method name = "fold_map"

    inherit reconstructors

    method class_params ~loc = [(ptyp_var ~loc "acc", (NoVariance, NoInjectivity))]

    method apply ~loc expr args = eapply ~loc expr (args @ [evar ~loc "acc"])
    method abstract ~loc patt expr = eabstract ~loc [patt; pvar ~loc "acc"] expr

    method typ ~loc ty = [%type: [%t ty] -> 'acc -> [%t ty] * 'acc]

    method any ~loc = [%expr fun x acc -> (x, acc)]

    method combine ~loc combinators ~reconstruct =
      List.fold_right combinators ~init:[%expr ([%e reconstruct], acc)]
        ~f:(fun (v, expr) acc ->
          [%expr
            let ([%p pvar_of_var v], acc) = [%e expr] in
            [%e acc]
          ])
  end

  exception Found
  let uses_var var =
    let iter = object
      inherit Ast_traverse.iter as super
      method! expression_desc = function
        | Pexp_ident { txt = Lident id; _ } when String.equal id var ->
          raise_notrace Found
        | e -> super#expression_desc e
    end in
    fun e ->
      try
        iter#expression e;
        false
      with Found ->
        true
  ;;

  let mapper_with_context : what =
    let uses_ctx = uses_var "ctx" in
    object
      method name = "map_with_context"

      inherit reconstructors

      method class_params ~loc = [(ptyp_var ~loc "ctx", (NoVariance, NoInjectivity))]

      method apply ~loc expr args = eapply ~loc expr (evar ~loc "ctx" :: args)
      method abstract ~loc patt expr =
        if uses_ctx expr then
          eabstract ~loc [pvar ~loc "ctx"; patt] expr
        else
          eabstract ~loc [pvar ~loc "_ctx"; patt] expr

      method typ ~loc ty = [%type: 'ctx -> [%t ty] -> [%t ty]]

      method any ~loc = [%expr fun _ctx x -> x]

      method combine ~loc combinators ~reconstruct =
        List.fold_right combinators ~init:reconstruct
          ~f:(fun (v, expr) acc ->
            [%expr
              let [%p pvar_of_var v] = [%e expr] in
              [%e acc]
            ])
    end

  let string_of_lid id = String.concat ~sep:"." (Longident.flatten_exn id)

  let lifter : what = object
    method name = "lift"

    method class_params ~loc = [(ptyp_var ~loc "res", (NoVariance, NoInjectivity))]

    method apply ~loc expr args = eapply ~loc expr args
    method abstract ~loc patt expr = pexp_fun ~loc Nolabel None patt expr

    method typ ~loc ty = [%type: [%t ty] -> 'res]

    method any ~loc = [%expr self#other]

    method combine ~loc combinators ~reconstruct =
      List.fold_right combinators ~init:reconstruct ~f:(fun (v, expr) acc ->
        pexp_let ~loc Nonrecursive [value_binding ~loc ~pat:(pvar_of_var v) ~expr] acc)

    method record ~loc flds =
      let flds =
        elist ~loc
          (List.map flds ~f:(fun (lab, e) ->
             pexp_tuple ~loc:{ lab.loc with loc_end = e.pexp_loc.loc_end }
               [ estring ~loc:lab.loc (string_of_lid lab.txt)
               ; e
               ]))
      in
      [%expr self#record [%e flds]]
    method construct ~loc id args =
      let args = elist ~loc args in
      [%expr self#constr [%e estring ~loc:id.loc (string_of_lid id.txt)] [%e args]]
    method tuple ~loc es =
      [%expr self#tuple [%e elist ~loc es]]
  end

  let all = [mapper; iterator; folder; fold_mapper; mapper_with_context; lifter]
end
type what = Backends.what

let mapper_type ~(what:what) ~loc type_name params =
  let vars = vars_of_list params ~get_loc:(fun t -> t.ptyp_loc) in
  let params = tvars_of_vars vars in
  let ty = ptyp_constr ~loc type_name params in
  let ty =
    List.fold_right params ~init:(what#typ ~loc ty)
      ~f:(fun param ty ->
        let loc = param.ptyp_loc in
        ptyp_arrow ~loc Nolabel (what#typ ~loc param) ty)
  in
  ptyp_poly ~loc vars ty
;;

let constrained_mapper ~(what:what) ?(is_gadt=false) mapper td =
  let vars = vars_of_list td.ptype_params ~get_loc:(fun (t, _) -> t.ptyp_loc) in
  let make_type params =
    let loc = td.ptype_loc in
    let ty = ptyp_constr ~loc (Loc.map td.ptype_name ~f:lident) params in
    List.fold_right params ~init:(what#typ ~loc:td.ptype_loc ty)
      ~f:(fun param ty ->
        let loc = param.ptyp_loc in
        ptyp_arrow ~loc Nolabel (what#typ ~loc param) ty)
  in
  let typ =
    let loc = td.ptype_loc in
    ptyp_poly ~loc vars (make_type (tvars_of_vars vars))
  in
  let mapper =
    if false || is_gadt then
      let typs =
        List.map vars ~f:(fun v -> ptyp_constr ~loc:v.loc (Loc.map v ~f:lident) [])
      in
      List.fold_right vars
        ~init:(pexp_constraint ~loc:mapper.pexp_loc mapper (make_type typs))
        ~f:(fun v e -> pexp_newtype ~loc:v.loc v e)
    else
      mapper
  in
  pexp_poly ~loc:mapper.pexp_loc mapper (Some typ)
;;

let mangle_type_name lid =
  let rec mangled_parts lid ~suffix =
    match lid with
    | Lident s -> String.lowercase_ascii s :: suffix
    | Ldot (lid, s) ->
      mangled_parts lid ~suffix:("__" :: String.lowercase_ascii s :: suffix)
    | Lapply (a, b) ->
      mangled_parts a ~suffix:("_'" :: mangled_parts b ~suffix:("'" :: suffix))
  in
  mangled_parts lid ~suffix:[] |> String.concat ~sep:""

let rec type_expr_mapper ~(what:what) te =
  let loc = te.ptyp_loc in
  match te.ptyp_desc with
  | Ptyp_var s -> evar ~loc ("_" ^ s)
  | Ptyp_tuple tes ->
    let vars = vars_of_list tes ~get_loc:(fun t -> t.ptyp_loc) in
    let deconstruct = ppat_tuple ~loc (pvars_of_vars vars) in
    let reconstruct = what#tuple ~loc (evars_of_vars vars) in
    let mappers = map_variables ~what vars tes in
    what#abstract ~loc deconstruct (what#combine ~loc mappers ~reconstruct)
  | Ptyp_constr (path, params) ->
    let map = pexp_send ~loc (evar ~loc "self") (Loc.map path ~f:mangle_type_name) in
    (match params with
     | [] -> map
     | _  ->
       eapply ~loc map
         (List.map params
            ~f:(fun te ->
              type_expr_mapper ~what te)))
  | _ -> what#any ~loc

and map_variables ~(what:what) vars tes =
  List.map2 tes vars ~f:(fun te var ->
    (var,
     what#apply ~loc:te.ptyp_loc (type_expr_mapper ~what te)
       [evar_of_var var]))
;;

let gen_record' ~(what:what) ~loc lds =
  let vars = List.map lds ~f:(fun ld -> ld.pld_name) in
  let deconstruct =
    ppat_record ~loc
      (List.map vars ~f:(fun v -> (Loc.map v ~f:lident, pvar_of_var v)))
      Closed
  in
  let reconstruct =
    what#record ~loc
      (List.map vars ~f:(fun v -> (Loc.map v ~f:lident, evar_of_var v)))
  in
  let mappers =
    map_variables ~what
      vars
      (List.map lds ~f:(fun ld -> ld.pld_type))
  in
  deconstruct, reconstruct, mappers
;;

let gen_record ~(what:what) ~loc lds =
  let deconstruct, reconstruct, mappers =
    gen_record' ~what lds ~loc
  in
  what#abstract ~loc deconstruct (what#combine ~loc mappers ~reconstruct)
;;

let is_constant_constructor cd =
  match cd.pcd_args with
  | Pcstr_tuple [] -> true
  | _ -> false

let erase_type_variables = object
  inherit Ast_traverse.map as super

  method! core_type_desc = function
    | Ptyp_var _ -> Ptyp_any
    | x -> super#core_type_desc x
end

let gen_variant ~(what:what) ~loc cds =
  if String.(<>) what#name "lift" &&
     List.for_all cds ~f:is_constant_constructor then
    what#any ~loc
  else
    let cases =
      List.map cds ~f:(fun cd ->
        let cstr = Loc.map cd.pcd_name ~f:lident in
        let loc = cd.pcd_loc in
        let args =
          match cd.pcd_res with
          | None -> cd.pcd_args
          | Some _ ->
            (* This is a big sur-approximation but it's enough for our only use of GADTs
               in ppx_custom_format *)
            erase_type_variables#constructor_arguments cd.pcd_args
        in
        match args with
        | Pcstr_tuple args ->
          let vars = vars_of_list args ~get_loc:(fun t -> t.ptyp_loc) in
          let deconstruct =
            ppat_construct cstr ~loc
              (match vars with
               | [] -> None
               | _ -> Some (ppat_tuple ~loc (pvars_of_vars vars)))
          in
          let reconstruct =
            what#construct cstr ~loc (evars_of_vars vars)
          in
          let mappers =
            map_variables ~what vars args
          in
          case ~lhs:deconstruct ~rhs:(what#combine ~loc mappers ~reconstruct) ~guard:None
        | Pcstr_record labels ->
          let deconstruct, reconstruct, mappers =
            gen_record' ~loc ~what labels
          in
          let deconstruct = ppat_construct ~loc cstr (Some deconstruct) in
          let reconstruct = what#construct ~loc cstr [reconstruct] in
          case ~lhs:deconstruct ~rhs:(what#combine ~loc mappers ~reconstruct) ~guard:None)
    in
    what#abstract ~loc (pvar ~loc "x") (pexp_match ~loc (evar ~loc "x") cases)

let gen_mapper ~(what:what) td =
  let body =
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_open -> what#any ~loc
    | Ptype_record  lds -> gen_record  ~what lds ~loc
    | Ptype_variant cds -> gen_variant ~what cds ~loc
    | Ptype_abstract ->
      match td.ptype_manifest with
      | None -> what#any ~loc
      | Some te -> type_expr_mapper ~what te
  in
  List.fold_right td.ptype_params ~init:body ~f:(fun (ty, _) acc ->
    let loc = ty.ptyp_loc in
    match ty.ptyp_desc with
    | Ptyp_var s ->
      pexp_fun ~loc Nolabel None (pvar ~loc ("_" ^ s)) acc
    | _ ->
      pexp_fun ~loc Nolabel None (ppat_any ~loc) acc)
;;

let type_deps =
  let collect = object
    inherit [int Longident.Map.t] Ast_traverse.fold as super
    method! core_type t acc =
      let acc =
        match t.ptyp_desc with
        | Ptyp_constr (id, vars) -> Longident.Map.add id.txt (List.length vars) acc
        | _ -> acc
      in
      super#core_type t acc
  end in
  fun tds ->
    let empty = Longident.Map.empty in
    let map =
      List.fold_left tds ~init:empty ~f:(fun map td ->
        let map = collect#type_kind td.ptype_kind map in
        match td.ptype_kind, td.ptype_manifest with
        | Ptype_abstract, Some ty -> collect#core_type ty map
        | _ -> map)
    in
    let map =
      List.fold_left tds ~init:map ~f:(fun map td ->
        Longident.Map.remove (Lident td.ptype_name.txt) map)
    in
    Longident.Map.bindings map

let lift_virtual_methods ~loc methods =
  let collect = object
    inherit [String.Set.t] Ast_traverse.fold as super

    method! expression_desc x acc =
      match x with
      | Pexp_send (_, ({ txt = "tuple"|"record"|"constr"|"other" as s; loc = _; })) ->
        String.Set.add s acc
      | _ -> super#expression_desc x acc
  end in
  let used = collect#list collect#class_field methods String.Set.empty in
  let all_virtual_methods =
    match
      [%stri
        class virtual blah = object
          method virtual record : (string * 'res) list -> 'res
          method virtual constr : string -> 'res list -> 'rest
          method virtual tuple : 'res list -> 'res
          method virtual other : 'a. 'a -> 'res
        end
      ]
    with
    | { pstr_desc =
          Pstr_class
            [ { pci_expr =
                  { pcl_desc =
                      Pcl_structure { pcstr_fields = l; _ }
                  ; _ }
              ; _ } ]
      ; _ } -> l
    | _ -> assert false
  in
  List.filter all_virtual_methods ~f:(fun m ->
    match m.pcf_desc with
    | Pcf_method (s, _, _) -> String.Set.mem s.txt used
    | _ -> false)

let gen_class ~(what:what) ~loc tds =
  let class_params = what#class_params ~loc in
  let virtual_methods =
    List.map (type_deps tds) ~f:(fun (id, arity) ->
      pcf_method ~loc
        ({ txt = mangle_type_name id; loc },
         Public,
         Cfk_virtual (mapper_type ~what ~loc {txt = id; loc}
                        (List.init ~len:arity ~f:(fun _ -> ptyp_any ~loc)))))
  in
  let methods =
    List.map tds ~f:(fun td ->
      let loc = td.ptype_loc in
      let mapper = gen_mapper ~what td in
      let is_gadt =
        match td.ptype_kind with
        | Ptype_variant cds -> List.exists cds ~f:(fun cd -> Option.is_some cd.pcd_res)
        | _ -> false
      in
      let mapper = constrained_mapper ~what ~is_gadt mapper td in
      pcf_method ~loc
        (td.ptype_name,
         Public,
         Cfk_concrete (Fresh, mapper)))
  in
  let virtual_methods =
    if String.equal what#name "lift" then
      lift_virtual_methods ~loc methods @ virtual_methods
    else
      virtual_methods
  in
  let virt = if List.is_empty virtual_methods then Concrete else Virtual in
  class_infos
    ~loc
    ~virt
    ~params:class_params
    ~name:{ loc; txt = what#name }
    ~expr:(pcl_structure ~loc
             (class_structure
                ~self:(ppat_var ~loc { txt = "self"; loc })
                ~fields:(virtual_methods @ methods)))

let gen_str ~(what:what)~loc ~path:_ (rf, tds) =
  (match rf with
   | Nonrecursive ->
     (* The method name would clash... *)
     Location.raise_errorf ~loc "ppxlib_traverse doesn't support nonrec"
   | Recursive -> ());
  let cl = gen_class ~loc ~what tds in
  [ pstr_class ~loc:cl.pci_loc [cl] ]

let () =
  let derivers =
    List.map Backends.all ~f:(fun what ->
      Deriving.add ("traverse_" ^ what#name)
        ~str_type_decl:(Deriving.Generator.make_noarg (gen_str ~what)))
  in
  Deriving.add_alias "traverse" (List.rev derivers)
  |> Deriving.ignore
