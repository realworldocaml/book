open! Import
open Common

type (_, _) equality = Eq : ('a, 'a) equality | Ne : (_, _) equality

module Context = struct
  type 'a t =
    | Class_expr : class_expr t
    | Class_field : class_field t
    | Class_type : class_type t
    | Class_type_field : class_type_field t
    | Core_type : core_type t
    | Expression : expression t
    | Module_expr : module_expr t
    | Module_type : module_type t
    | Pattern : pattern t
    | Signature_item : signature_item t
    | Structure_item : structure_item t
    | Ppx_import : type_declaration t

  type packed = T : _ t -> packed

  let class_expr = Class_expr
  let class_field = Class_field
  let class_type = Class_type
  let class_type_field = Class_type_field
  let core_type = Core_type
  let expression = Expression
  let module_expr = Module_expr
  let module_type = Module_type
  let pattern = Pattern
  let signature_item = Signature_item
  let structure_item = Structure_item

  let desc : type a. a t -> string = function
    | Class_expr -> "class expression"
    | Class_field -> "class field"
    | Class_type -> "class type"
    | Class_type_field -> "class type field"
    | Core_type -> "core type"
    | Expression -> "expression"
    | Module_expr -> "module expression"
    | Module_type -> "module type"
    | Pattern -> "pattern"
    | Signature_item -> "signature item"
    | Structure_item -> "structure item"
    | Ppx_import -> "type declaration"

  let eq : type a b. a t -> b t -> (a, b) equality =
   fun a b ->
    match (a, b) with
    | Class_expr, Class_expr -> Eq
    | Class_field, Class_field -> Eq
    | Class_type, Class_type -> Eq
    | Class_type_field, Class_type_field -> Eq
    | Core_type, Core_type -> Eq
    | Expression, Expression -> Eq
    | Module_expr, Module_expr -> Eq
    | Module_type, Module_type -> Eq
    | Pattern, Pattern -> Eq
    | Signature_item, Signature_item -> Eq
    | Structure_item, Structure_item -> Eq
    | Ppx_import, Ppx_import -> Eq
    | _ ->
        assert (Poly.( <> ) (T a) (T b));
        Ne

  let get_ppx_import_extension type_decl =
    match type_decl with
    | { ptype_manifest = Some { ptyp_desc = Ptyp_extension (name, _); _ }; _ }
      ->
        let virtual_payload =
          Ast_builder.Default.pstr_type ~loc:type_decl.ptype_loc Recursive
            [ type_decl ]
        in
        let attr = [] in
        Some ((name, PStr [ virtual_payload ]), attr)
    | _ -> None

  let get_extension : type a. a t -> a -> (extension * attributes) option =
   fun t x ->
    match (t, x) with
    | Class_expr, { pcl_desc = Pcl_extension e; pcl_attributes = a; _ } ->
        Some (e, a)
    | Class_field, { pcf_desc = Pcf_extension e; pcf_attributes = a; _ } ->
        Some (e, a)
    | Class_type, { pcty_desc = Pcty_extension e; pcty_attributes = a; _ } ->
        Some (e, a)
    | Class_type_field, { pctf_desc = Pctf_extension e; pctf_attributes = a; _ }
      ->
        Some (e, a)
    | Core_type, { ptyp_desc = Ptyp_extension e; ptyp_attributes = a; _ } ->
        Some (e, a)
    | Expression, { pexp_desc = Pexp_extension e; pexp_attributes = a; _ } ->
        Some (e, a)
    | Module_expr, { pmod_desc = Pmod_extension e; pmod_attributes = a; _ } ->
        Some (e, a)
    | Module_type, { pmty_desc = Pmty_extension e; pmty_attributes = a; _ } ->
        Some (e, a)
    | Pattern, { ppat_desc = Ppat_extension e; ppat_attributes = a; _ } ->
        Some (e, a)
    | Signature_item, { psig_desc = Psig_extension (e, a); _ } -> Some (e, a)
    | Structure_item, { pstr_desc = Pstr_extension (e, a); _ } -> Some (e, a)
    | Ppx_import, type_decl -> get_ppx_import_extension type_decl
    | _ -> None

  let node_of_extension :
      type a. ?loc:Location.t -> ?x:a -> a t -> extension -> a =
   fun ?(loc = Location.none) ?x t ->
    let open Ast_builder.Default in
    match (t, x) with
    | Class_expr, _ -> pcl_extension ~loc
    | Class_field, _ -> pcf_extension ~loc
    | Class_type_field, _ -> pctf_extension ~loc
    | Class_type, _ -> pcty_extension ~loc
    | Core_type, _ -> ptyp_extension ~loc
    | Expression, _ -> pexp_extension ~loc
    | Module_expr, _ -> pmod_extension ~loc
    | Module_type, _ -> pmty_extension ~loc
    | Pattern, _ -> ppat_extension ~loc
    | Signature_item, _ -> fun ext -> psig_extension ~loc ext []
    | Structure_item, _ -> fun ext -> pstr_extension ~loc ext []
    | Ppx_import, Some x ->
        fun ext ->
          {
            x with
            ptype_manifest = Some (Ast_builder.Default.ptyp_extension ~loc ext);
          }
    | Ppx_import, None ->
        failwith
          "Ppxlib internal error: Item not provided to build an extension node \
           from a Ppx_import context."

  let merge_attributes_res :
      type a.
      a t -> a -> attributes -> (a, Location.Error.t NonEmptyList.t) result =
   fun t x attrs ->
    match t with
    | Class_expr -> Ok { x with pcl_attributes = x.pcl_attributes @ attrs }
    | Class_field -> Ok { x with pcf_attributes = x.pcf_attributes @ attrs }
    | Class_type -> Ok { x with pcty_attributes = x.pcty_attributes @ attrs }
    | Class_type_field ->
        Ok { x with pctf_attributes = x.pctf_attributes @ attrs }
    | Core_type -> Ok { x with ptyp_attributes = x.ptyp_attributes @ attrs }
    | Expression -> Ok { x with pexp_attributes = x.pexp_attributes @ attrs }
    | Module_expr -> Ok { x with pmod_attributes = x.pmod_attributes @ attrs }
    | Module_type -> Ok { x with pmty_attributes = x.pmty_attributes @ attrs }
    | Pattern -> Ok { x with ppat_attributes = x.ppat_attributes @ attrs }
    | Signature_item -> (
        match attributes_errors attrs with [] -> Ok x | t :: q -> Error (t, q))
    | Structure_item -> (
        match attributes_errors attrs with [] -> Ok x | t :: q -> Error (t, q))
    | Ppx_import -> (
        match attributes_errors attrs with [] -> Ok x | t :: q -> Error (t, q))

  let merge_attributes : type a. a t -> a -> attributes -> a =
   fun t x attrs ->
    merge_attributes_res t x attrs
    |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err)
end

let registrar =
  Name.Registrar.create ~kind:"extension" ~current_file:__FILE__
    ~string_of_context:(fun (Context.T ctx) -> Some (Context.desc ctx))

module Make (Callback : sig
  type 'a t
end) =
struct
  type ('a, 'b) payload_parser =
    | Payload_parser :
        ('a, 'b, 'c) Ast_pattern.t * 'b Callback.t
        -> ('a, 'c) payload_parser

  type ('context, 'payload) t = {
    name : Name.Pattern.t;
    context : 'context Context.t;
    payload : (payload, 'payload) payload_parser;
    with_arg : bool;
  }

  let declare :
      type a.
      with_arg:bool ->
      string ->
      a Context.t ->
      (payload, 'b, 'payload) Ast_pattern.t ->
      'b Callback.t ->
      (a, 'payload) t =
   fun ~with_arg name context pattern k ->
    (* Check that there is no collisions between ppx_import and core_type
       extensions *)
    (match context with
    | Context.Ppx_import ->
        Name.Registrar.check_collisions registrar (Context.T Core_type) name
    | Context.Core_type ->
        Name.Registrar.check_collisions registrar (Context.T Ppx_import) name
    | _ -> ());
    Name.Registrar.register ~kind:`Extension registrar (Context.T context) name;
    {
      name = Name.Pattern.make name;
      context;
      payload = Payload_parser (pattern, k);
      with_arg;
    }

  let find ts (ext : extension) =
    let { txt = name; loc } = fst ext in
    let name, arg = Name.split_path name in
    match List.filter ts ~f:(fun t -> Name.Pattern.matches t.name name) with
    | [] -> Ok None
    | _ :: _ :: _ as l ->
        Error
          ( Location.Error.createf ~loc "Multiple match for extensions: %s"
              (String.concat ~sep:", "
                 (List.map l ~f:(fun t -> Name.Pattern.name t.name))),
            [] )
    | [ t ] ->
        if (not t.with_arg) && Option.is_some arg then
          Error
            ( Location.Error.createf ~loc
                "Extension %s doesn't expect a path argument" name,
              [] )
        else
          let arg =
            Option.map arg ~f:(fun s ->
                let shift = String.length name + 1 in
                let start = loc.loc_start in
                {
                  txt = Longident.parse s;
                  loc =
                    {
                      loc with
                      loc_start =
                        { start with pos_cnum = start.pos_cnum + shift };
                    };
                })
          in
          Ok (Some (t, arg))
end

module Expert = struct
  include Make (struct
    type 'a t = arg:Longident.t Loc.t option -> 'a
  end)

  let declare_with_path_arg name ctx patt f =
    declare ~with_arg:true name ctx patt f

  let declare name ctx patt f =
    declare ~with_arg:false name ctx patt (fun ~arg:_ -> f)

  let convert_res ts ~loc ext =
    let open Result in
    find ts ext >>= fun r ->
    match r with
    | None -> Ok None
    | Some ({ payload = Payload_parser (pattern, f); _ }, arg) ->
        Ast_pattern.parse_res pattern loc (snd ext) (f ~arg) >>| fun payload ->
        Some payload

  let convert ts ~loc ext =
    convert_res ts ~loc ext
    |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err)
end

module M = Make (struct
  type 'a t =
    ctxt:Expansion_context.Extension.t -> arg:Longident.t Loc.t option -> 'a
end)

type 'a expander_result = Simple of 'a | Inline of 'a list

module For_context = struct
  type 'a t = ('a, 'a expander_result) M.t

  let convert_res ts ~ctxt ext =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let open Result in
    M.find ts ext >>= fun found ->
    match found with
    | None -> Ok None
    | Some ({ payload = M.Payload_parser (pattern, f); _ }, arg) -> (
        Ast_pattern.parse_res pattern loc (snd ext) (f ~ctxt ~arg)
        >>| fun payload ->
        match payload with
        | Simple x -> Some x
        | Inline _ -> failwith "Extension.convert")

  let convert ts ~ctxt ext =
    convert_res ts ~ctxt ext
    |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err)

  let convert_inline_res ts ~ctxt ext =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let open Result in
    M.find ts ext >>= fun found ->
    match found with
    | None -> Ok None
    | Some ({ payload = M.Payload_parser (pattern, f); _ }, arg) -> (
        Ast_pattern.parse_res pattern loc (snd ext) (f ~ctxt ~arg)
        >>| fun payload ->
        match payload with Simple x -> Some [ x ] | Inline l -> Some l)

  let convert_inline ts ~ctxt ext =
    convert_inline_res ts ~ctxt ext
    |> Result.handle_error ~f:(fun (err, _) -> Location.Error.raise err)
end

type t = T : _ For_context.t -> t

let check_context_for_inline : type a. func:string -> a Context.t -> unit =
 fun ~func ctx ->
  match ctx with
  | Context.Class_field -> ()
  | Context.Class_type_field -> ()
  | Context.Signature_item -> ()
  | Context.Structure_item -> ()
  | context ->
      Printf.ksprintf invalid_arg "%s: %s can't be inlined" func
        (Context.desc context)

let rec filter_by_context :
    type a. a Context.t -> t list -> a For_context.t list =
 fun context expanders ->
  match expanders with
  | [] -> []
  | T t :: rest -> (
      match Context.eq context t.context with
      | Eq -> t :: filter_by_context context rest
      | Ne -> filter_by_context context rest)

let unhandled_extension_error ctx (name, _) =
  if
    not
      (Name.Allowlisted.is_allowlisted ~kind:`Extension name.txt
      || Name.ignore_checks name.txt)
  then
    [
      Name.Registrar.Error.createf registrar (Context.T ctx)
        "Extension `%s' was not translated" name;
    ]
  else []

let collect_unhandled_extension_errors =
  object
    inherit [Location.Error.t list] Ast_traverse.fold as super

    method! extension (name, _) acc =
      acc
      @ [
          Location.Error.createf ~loc:name.loc
            "extension not expected here, Ppxlib.Extension needs updating!";
        ]

    method! core_type_desc x acc =
      match x with
      | Ptyp_extension ext -> acc @ unhandled_extension_error Core_type ext
      | x -> super#core_type_desc x acc

    method! pattern_desc x acc =
      match x with
      | Ppat_extension ext -> acc @ unhandled_extension_error Pattern ext
      | x -> super#pattern_desc x acc

    method! expression_desc x acc =
      match x with
      | Pexp_extension ext -> acc @ unhandled_extension_error Expression ext
      | x -> super#expression_desc x acc

    method! class_type_desc x acc =
      match x with
      | Pcty_extension ext -> acc @ unhandled_extension_error Class_type ext
      | x -> super#class_type_desc x acc

    method! class_type_field_desc x acc =
      match x with
      | Pctf_extension ext ->
          acc @ unhandled_extension_error Class_type_field ext
      | x -> super#class_type_field_desc x acc

    method! class_expr_desc x acc =
      match x with
      | Pcl_extension ext -> acc @ unhandled_extension_error Class_expr ext
      | x -> super#class_expr_desc x acc

    method! class_field_desc x acc =
      match x with
      | Pcf_extension ext -> acc @ unhandled_extension_error Class_field ext
      | x -> super#class_field_desc x acc

    method! module_type_desc x acc =
      match x with
      | Pmty_extension ext -> acc @ unhandled_extension_error Module_type ext
      | x -> super#module_type_desc x acc

    method! signature_item_desc x acc =
      match x with
      | Psig_extension (ext, _) ->
          acc @ unhandled_extension_error Signature_item ext
      | x -> super#signature_item_desc x acc

    method! module_expr_desc x acc =
      match x with
      | Pmod_extension ext -> acc @ unhandled_extension_error Module_expr ext
      | x -> super#module_expr_desc x acc

    method! structure_item_desc x acc =
      match x with
      | Pstr_extension (ext, _) ->
          acc @ unhandled_extension_error Structure_item ext
      | x -> super#structure_item_desc x acc
  end

let error_list_to_exception = function
  | [] -> ()
  | err :: _ -> Location.Error.raise err

let check_unused =
  object
    inherit Ast_traverse.iter

    method! extension (name, _) =
      Location.raise_errorf ~loc:name.loc
        "extension not expected here, Ppxlib.Extension needs updating!"

    method! core_type_desc x =
      collect_unhandled_extension_errors#core_type_desc x []
      |> error_list_to_exception

    method! pattern_desc x =
      collect_unhandled_extension_errors#pattern_desc x []
      |> error_list_to_exception

    method! expression_desc x =
      collect_unhandled_extension_errors#expression_desc x []
      |> error_list_to_exception

    method! class_type_desc x =
      collect_unhandled_extension_errors#class_type_desc x []
      |> error_list_to_exception

    method! class_type_field_desc x =
      collect_unhandled_extension_errors#class_type_field_desc x []
      |> error_list_to_exception

    method! class_expr_desc x =
      collect_unhandled_extension_errors#class_expr_desc x []
      |> error_list_to_exception

    method! class_field_desc x =
      collect_unhandled_extension_errors#class_field_desc x []
      |> error_list_to_exception

    method! module_type_desc x =
      collect_unhandled_extension_errors#module_type_desc x []
      |> error_list_to_exception

    method! signature_item_desc x =
      collect_unhandled_extension_errors#signature_item_desc x []
      |> error_list_to_exception

    method! module_expr_desc x =
      collect_unhandled_extension_errors#module_expr_desc x []
      |> error_list_to_exception

    method! structure_item_desc x =
      collect_unhandled_extension_errors#structure_item_desc x []
      |> error_list_to_exception
  end

module V3 = struct
  type nonrec t = t

  let declare name context pattern k =
    let pattern = Ast_pattern.map_result pattern ~f:(fun x -> Simple x) in
    T
      (M.declare ~with_arg:false name context pattern (fun ~ctxt ~arg:_ ->
           k ~ctxt))

  let declare_inline name context pattern k =
    check_context_for_inline context ~func:"Extension.declare_inline";
    let pattern = Ast_pattern.map_result pattern ~f:(fun x -> Inline x) in
    T
      (M.declare ~with_arg:false name context pattern (fun ~ctxt ~arg:_ ->
           k ~ctxt))
end

let declare name context pattern f =
  V3.declare name context pattern
    (Expansion_context.Extension.with_loc_and_path f)

let declare_inline name context pattern f =
  V3.declare_inline name context pattern
    (Expansion_context.Extension.with_loc_and_path f)

let declare_with_path_arg name context pattern k =
  let k' = Expansion_context.Extension.with_loc_and_path k in
  let pattern = Ast_pattern.map_result pattern ~f:(fun x -> Simple x) in
  T (M.declare ~with_arg:true name context pattern k')

let declare_inline_with_path_arg name context pattern k =
  let k' = Expansion_context.Extension.with_loc_and_path k in
  check_context_for_inline context
    ~func:"Extension.declare_inline_with_path_arg";
  let pattern = Ast_pattern.map_result pattern ~f:(fun x -> Inline x) in
  T (M.declare ~with_arg:true name context pattern k')

let __declare_ppx_import name expand =
  (* This pattern is used to unwrap the type declaration from the payload
     assembled by [Context.get_ppx_import_extension] *)
  let pattern = Ast_pattern.(pstr (pstr_type recursive (__ ^:: nil) ^:: nil)) in
  V3.declare name Context.Ppx_import pattern expand

module V2 = struct
  type nonrec t = t

  let declare = declare
  let declare_inline = declare_inline
end
