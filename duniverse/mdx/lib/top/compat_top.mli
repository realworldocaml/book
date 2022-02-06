val lookup_type : Longident.t -> Env.t -> Path.t
val lookup_value : Longident.t -> Env.t -> Path.t * Types.value_description

val find_value :
  Env.t -> Location.t -> Longident.t -> Path.t * Types.value_description

val find_type :
  Env.t -> Location.t -> Longident.t -> Path.t * Types.type_declaration

val find_constructor :
  Env.t -> Location.t -> Longident.t -> Types.constructor_description

val find_module :
  Env.t -> Location.t -> Longident.t -> Path.t * Types.module_declaration

val find_modtype :
  Env.t -> Location.t -> Longident.t -> Path.t * Types.modtype_declaration

val find_class :
  Env.t -> Location.t -> Longident.t -> Path.t * Types.class_declaration

val find_class_type :
  Env.t -> Location.t -> Longident.t -> Path.t * Types.class_type_declaration

val type_structure :
  Env.t -> Parsetree.structure -> Location.t -> Typedtree.structure * Env.t

val extension_constructor :
  ext_type_path:Path.t ->
  ext_type_params:Types.type_expr list ->
  ext_args:Types.type_expr list ->
  ext_ret_type:Types.type_expr option ->
  ext_private:Asttypes.private_flag ->
  ext_loc:Location.t ->
  ext_attributes:Parsetree.attributes ->
  Types.extension_constructor

val match_env :
  value:(Env.summary -> Ident.t -> 'a) ->
  empty:(unit -> 'a) ->
  open_:(Env.summary -> Path.t -> 'a) ->
  functor_arg:(Env.summary -> Ident.t -> 'a) ->
  constraints:(Env.summary -> 'a) ->
  copy_types:(Env.summary -> 'a) ->
  module_:(Env.summary -> Ident.t -> present:bool -> 'a) ->
  persistent:(Env.summary -> 'a) ->
  type_:(Env.summary -> 'a) ->
  modtype:(Env.summary -> 'a) ->
  cltype:(Env.summary -> 'a) ->
  class_:(Env.summary -> Ident.t -> 'a) ->
  extension:(Env.summary -> Ident.t -> 'a) ->
  value_unbound:(Env.summary -> 'a) ->
  module_unbound:(Env.summary -> 'a) ->
  Env.summary ->
  'a

val ctype_is_equal :
  Env.t -> bool -> Types.type_expr list -> Types.type_expr list -> bool

val ctype_expand_head_and_get_desc : Env.t -> Types.type_expr -> Types.type_desc
val ctype_get_desc : Types.type_expr -> Types.type_desc
