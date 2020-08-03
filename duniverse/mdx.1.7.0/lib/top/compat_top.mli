open Mdx.Migrate_ast

val try_finally : always:(unit -> unit) -> (unit -> 'a) -> 'a

val map_error_loc :
  f:(Location.t -> Location.t) -> Location.error -> Location.error

val error_of_exn : exn -> Location.error option

val get_id_in_path : Path.t -> Ident.t

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

val sig_value : Ident.t -> Types.value_description -> Types.signature_item

val sig_type : Ident.t -> Types.type_declaration -> Types.signature_item

val sig_typext : Ident.t -> Types.extension_constructor -> Types.signature_item

val sig_module : Ident.t -> Types.module_declaration -> Types.signature_item

val mty_path : Types.module_type -> Path.t option

val sig_modtype : Ident.t -> Types.modtype_declaration -> Types.signature_item

val sig_class : Ident.t -> Types.class_declaration -> Types.signature_item

val sig_class_type :
  Ident.t -> Types.class_type_declaration -> Types.signature_item

val add_directive :
  name:string ->
  doc:string ->
  [ `Bool of bool -> unit
  | `Show_prim of
    Env.t -> Location.t -> Ident.t -> Longident.t -> Types.signature ] ->
  unit

val extension_constructor :
  ext_type_path:Path.t ->
  ext_type_params:Types.type_expr list ->
  ext_args:Types.type_expr list ->
  ext_ret_type:Types.type_expr option ->
  ext_private:Asttypes_.private_flag ->
  ext_loc:Location.t ->
  ext_attributes:Parsetree_.attributes ->
  Types.extension_constructor

val is_predef_or_global : Ident.t -> bool

val map_sig_attributes :
  f:(Parsetree_.attributes -> Parsetree_.attributes) ->
  Types.signature ->
  Types.signature

val attribute :
  name:string Location.loc -> payload:Parsetree_.payload -> Parsetree_.attribute

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
