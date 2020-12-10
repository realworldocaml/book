open Mdx.Migrate_ast

#if OCAML_VERSION >= (4, 8, 0)
let try_finally ~always f = Misc.try_finally f ~always
#else
let try_finally ~always f = Misc.try_finally f always
#endif

let map_error_loc ~f (error : Location.error) =
#if OCAML_VERSION >= (4, 8, 0)
  let f_msg (msg : Location.msg) =
    { msg with loc = f msg.loc}
  in
  { error with main = f_msg error.main;
                sub = List.map f_msg error.sub; }
#else
  let rec aux (error : Location.error) =
     {error with sub = List.map aux error.sub;
                 loc = f error.loc}
  in
  aux error
#endif

let error_of_exn exn =
#if OCAML_VERSION >= (4, 6, 0)
  match Location.error_of_exn exn with
    | None -> None
    | Some `Already_displayed -> None
    | Some (`Ok error) -> Some error
#else
  Location.error_of_exn exn
#endif

let rec get_id_in_path = function
  | Path.Pident id -> id
#if OCAML_VERSION >= (4, 8, 0)
  | Path.Pdot (p, _) -> get_id_in_path p
#else
  | Path.Pdot (p, _, _) -> get_id_in_path p
#endif
  | Path.Papply (_, p) -> get_id_in_path p

let lookup_type typ env =
#if OCAML_VERSION >= (4, 10, 0)
  Env.find_type_by_name typ env |> fst
#elif OCAML_VERSION >= (4, 4, 0)
  Env.lookup_type typ env
#else
  Env.lookup_type typ env |> fst
#endif

let lookup_value v env =
#if OCAML_VERSION >= (4, 10, 0)
  Env.find_value_by_name v env
#else
  Env.lookup_value v env
#endif

let find_value env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_value ~loc id env
#else
  Typetexp.find_value env loc id
#endif

let find_type env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_type ~loc id env
#else
  Typetexp.find_type env loc id
#endif

let find_constructor env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_constructor ~loc Env.Positive id env
#else
  Typetexp.find_constructor env loc id
#endif

let find_module env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_module ~loc id env
#else
  Typetexp.find_module env loc id
#endif

let find_modtype env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_modtype ~loc id env
#else
  Typetexp.find_modtype env loc id
#endif

let find_class env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_class ~loc id env
#else
  Typetexp.find_class env loc id
#endif

let find_class_type env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_cltype ~loc id env
#else
  Typetexp.find_class_type env loc id
#endif

let type_structure env str loc =
#if OCAML_VERSION >= (4, 8, 0)
  let tstr, _, _, env =
#else
  let tstr, _, env =
#endif
    Typemod.type_structure env str loc
  in
  tstr, env

let sig_value id desc =
#if OCAML_VERSION >= (4, 8, 0)
  Types.Sig_value (id, desc, Exported)
#else
  Types.Sig_value (id, desc)
#endif

let sig_type id desc =
#if OCAML_VERSION >= (4, 8, 0)
  Types.Sig_type (id, desc, Trec_not, Exported)
#else
  Types.Sig_type (id, desc, Trec_not)
#endif

let sig_typext id ext =
#if OCAML_VERSION >= (4, 8, 0)
  Types.Sig_typext (id, ext, Text_exception, Exported)
#else
  Types.Sig_typext (id, ext, Text_exception)
#endif

let sig_module id md =
#if OCAML_VERSION >= (4, 8, 0)
  Types.Sig_module (id, Mp_present, md, Trec_not, Exported)
#else
  Types.Sig_module (id, md, Trec_not)
#endif

let mty_path =
  let open Types in
  function
#if OCAML_VERSION >= (4, 4, 0) && OCAML_VERSION < (4, 8, 0)
  | Mty_alias (_, path) -> Some path
#else
  | Mty_alias path -> Some path
#endif
  | Mty_ident _
  | Mty_signature _
  | Mty_functor _ ->
    None

let sig_modtype id desc =
#if OCAML_VERSION >= (4, 8, 0)
  Types.Sig_modtype (id, desc, Exported)
#else
  Types.Sig_modtype (id, desc)
#endif

let sig_class id desc =
#if OCAML_VERSION >= (4, 8, 0)
  Types.Sig_class (id, desc, Trec_not, Exported)
#else
  Types.Sig_class (id, desc, Trec_not)
#endif

let sig_class_type id desc =
#if OCAML_VERSION >= (4, 8, 0)
  Types.Sig_class_type (id, desc, Trec_not, Exported)
#else
  Types.Sig_class_type (id, desc, Trec_not)
#endif

let add_directive ~name ~doc kind =
#if OCAML_VERSION >= (4, 3, 0)
  let directive = match kind with
    | `Bool f -> Toploop.Directive_bool f
    | `Show_prim to_sig ->
        let show_prim to_sig lid =
          let env = !Toploop.toplevel_env in
          let loc = Location.none in
          try
            let s =
              match lid with
              | Longident.Lident s -> s
              | Longident.Ldot (_,s) -> s
              | Longident.Lapply _ ->
                  Format.printf "Invalid path %a@." Printtyp.longident lid;
                  raise Exit
            in
            let id = Ident.create_persistent s in
            let sg = to_sig env loc id lid in
            Printtyp.wrap_printing_env env (fun () ->
                Format.printf "@[%a@]@." Printtyp.signature sg
              )
          with
          | Not_found -> Format.printf "@[Unknown element.@]@."
          | Exit -> ()
        in
    (Toploop.Directive_ident (show_prim to_sig))
  in
  Toploop.add_directive name
    directive
    { section = "Environment queries"; doc }
#else
  ignore (name, doc, kind)
#endif

let extension_constructor
    ~ext_type_path
    ~ext_type_params
    ~ext_args
    ~ext_ret_type
    ~ext_private
    ~ext_loc
    ~ext_attributes
  =
  let open Types in
  let ext_args =
#if OCAML_VERSION >= (4, 3, 0)
    Cstr_tuple ext_args
#else
    ext_args
#endif
  in
  { ext_type_path
  ; ext_type_params
  ; ext_args
  ; ext_ret_type
  ; ext_private
  ; ext_loc
  ; ext_attributes
#if OCAML_VERSION >= (4, 11, 0)
  ; ext_uid = Uid.mk ~current_unit:"mdx"
#endif
  }

let is_predef_or_global id =
#if OCAML_VERSION >= (4, 8, 0)
  Ident.is_predef id || Ident.global id
#else
  Ident.binding_time id < 1000
#endif

let map_sig_attributes ~f =
  let open Types in
  List.map (function
#if OCAML_VERSION >= (4, 8, 0)
    | Sig_module (id, mp, md, rs, visibility) ->
      Sig_module (
        id,
        mp,
        {md with md_attributes = f md.md_attributes },
        rs,
        visibility
      )
#else
    | Sig_module (id, md, rs) ->
      Sig_module (
        id,
        {md with md_attributes = f md.md_attributes},
        rs
      )
#endif
    | item -> item
)

let attribute ~name ~payload =
#if OCAML_VERSION >= (4, 8, 0)
  { Parsetree_.attr_name = name
  ; attr_payload = payload
  ; attr_loc = Location.none
  }
#else
  (name, payload)
#endif

module Linked = struct
  include (Topdirs : sig end)
#if OCAML_VERSION >= (4, 3, 0)
  include (Ephemeron : sig end)
#endif
  include (Uchar : sig end)
  include (Condition : sig end)
end

let match_env
    ~value
    ~empty
    ~open_
    ~functor_arg
    ~constraints
    ~copy_types
    ~module_
    ~persistent
    ~type_
    ~modtype
    ~cltype
    ~class_
    ~extension
    ~value_unbound
    ~module_unbound
    env =
  ignore (constraints, persistent, copy_types, value_unbound, module_unbound);
  match env with
  | Env.Env_value (summary, id, _) ->
    value summary id
  | Env_empty -> empty ()
#if OCAML_VERSION >= (4, 7, 0) && OCAML_VERSION < (4, 8, 0)
  | Env_open (summary, _, pid) ->
#else
  | Env_open (summary, pid) ->
#endif
    open_ summary pid
  | Env_functor_arg (summary, id) -> functor_arg summary id
#if OCAML_VERSION >= (4, 8, 0)
  | Env_module (summary, id, presence, _) ->
    let present = match presence with
      | Mp_present -> true
      | Mp_absent -> false
    in
    module_ summary id ~present
#else
  | Env_module (summary, id, _) ->
    module_ summary id ~present:true
#endif
  | Env_type (summary, _, _) -> type_ summary
  | Env_modtype (summary, _, _) -> modtype summary
  | Env_cltype (summary, _, _) -> cltype summary
  | Env_class (summary, id, _) -> class_ summary id
  | Env_extension (summary, id, _) -> extension summary id
#if OCAML_VERSION >= (4, 4, 0)
  | Env_constraints (summary, _) -> constraints summary
#endif
#if OCAML_VERSION >= (4, 10, 0)
  | Env_copy_types summary -> copy_types summary
  | Env_value_unbound (summary, _, _) -> value_unbound summary
  | Env_module_unbound (summary, _, _) -> module_unbound summary
#elif OCAML_VERSION >= (4, 6, 0)
  | Env_copy_types (summary, _) -> copy_types summary
#endif
#if OCAML_VERSION >= (4, 8, 0)
  | Env_persistent (summary, _) -> persistent summary
#endif
