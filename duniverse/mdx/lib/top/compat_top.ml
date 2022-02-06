let lookup_type typ env =
#if OCAML_VERSION >= (4, 10, 0)
  Env.find_type_by_name typ env |> fst
#else
  Env.lookup_type typ env
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
#if OCAML_VERSION >= (4, 14, 0)
  let tstr, _, _, _, env =
#else
  let tstr, _, _, env =
#endif
#if OCAML_VERSION >= (4, 12, 0)
    let _ = loc in
    Typemod.type_structure env str
#else
    Typemod.type_structure env str loc
#endif
  in
  tstr, env

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
    Cstr_tuple ext_args
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
  | Env_open (summary, pid) ->
    open_ summary pid
  | Env_functor_arg (summary, id) -> functor_arg summary id
  | Env_module (summary, id, presence, _) ->
    let present = match presence with
      | Mp_present -> true
      | Mp_absent -> false
    in
    module_ summary id ~present
  | Env_type (summary, _, _) -> type_ summary
  | Env_modtype (summary, _, _) -> modtype summary
  | Env_cltype (summary, _, _) -> cltype summary
  | Env_class (summary, id, _) -> class_ summary id
  | Env_extension (summary, id, _) -> extension summary id
  | Env_constraints (summary, _) -> constraints summary
#if OCAML_VERSION >= (4, 10, 0)
  | Env_copy_types summary -> copy_types summary
  | Env_value_unbound (summary, _, _) -> value_unbound summary
  | Env_module_unbound (summary, _, _) -> module_unbound summary
#else
  | Env_copy_types (summary, _) -> copy_types summary
#endif
  | Env_persistent (summary, _) -> persistent summary

let ctype_is_equal =
#if OCAML_VERSION >= (4, 13, 0)
  Ctype.is_equal
#else
  Ctype.equal
#endif

let ctype_expand_head_and_get_desc env ty =
#if OCAML_VERSION >= (4, 14, 0)
  Types.get_desc (Ctype.expand_head env ty)
#else
  (Ctype.expand_head env ty).Types.desc
#endif

let ctype_get_desc ty =
#if OCAML_VERSION >= (4, 14, 0)
  Types.get_desc ty
#else
  (Ctype.repr ty).Types.desc
#endif
