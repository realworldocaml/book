open! Import

type (_, _) equality = Eq : ('a, 'a) equality | Ne : (_, _) equality

module Context : sig
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
        (** For ppx_import compat only, please do not use *)

  val class_expr : class_expr t
  val class_field : class_field t
  val class_type : class_type t
  val class_type_field : class_type_field t
  val core_type : core_type t
  val expression : expression t
  val module_expr : module_expr t
  val module_type : module_type t
  val pattern : pattern t
  val signature_item : signature_item t
  val structure_item : structure_item t
  val eq : 'a t -> 'b t -> ('a, 'b) equality
  val get_extension : 'a t -> 'a -> (extension * attributes) option
  val merge_attributes : 'a t -> 'a -> attributes -> 'a

  val merge_attributes_res :
    'a t -> 'a -> attributes -> ('a, Location.Error.t NonEmptyList.t) result
end

type t
(** Type of declared extensions. *)

val declare :
  string ->
  'context Context.t ->
  (payload, 'a, 'context) Ast_pattern.t ->
  (loc:Location.t -> path:string -> 'a) ->
  t
(** [declare name context pattern expander] declares the extension names [name]
    for [context].

    [expander] is responsible for producing the code to replace the extension in
    the AST. It receives as argument:

    - [loc]: the location of the enclosing node. For instance for expression it
      is the [pexp_loc] field
    - [path]: the current module path *)

val declare_with_path_arg :
  string ->
  'context Context.t ->
  (payload, 'a, 'context) Ast_pattern.t ->
  (loc:Location.t -> path:string -> arg:Longident.t Asttypes.loc option -> 'a) ->
  t
(** Same as [declare] except that the extension name takes an additional path
    argument. The path is the part of the name that start with a capitalized
    component. For instance in the following, the extension ["map"] would
    receive the path argument [Foo.Bar]:

    {[
      let%map.Foo.Bar x = 1 in
      ...
    ]} *)

val declare_inline :
  string ->
  'context Context.t ->
  (payload, 'a, 'context list) Ast_pattern.t ->
  (loc:Location.t -> path:string -> 'a) ->
  t
(** Inline the result of the expansion into its parent. Only works for these
    contexts:

    - [class_field]
    - [class_type_field]
    - [signature_item]
    - [structure_item] *)

val declare_inline_with_path_arg :
  string ->
  'context Context.t ->
  (payload, 'a, 'context list) Ast_pattern.t ->
  (loc:Location.t -> path:string -> arg:Longident.t Asttypes.loc option -> 'a) ->
  t

module For_context : sig
  (** This module is used to implement {!Context_free.map_top_down} *)

  type 'a t

  val convert_res :
    'a t list ->
    ctxt:Expansion_context.Extension.t ->
    extension ->
    ('a option, Location.Error.t NonEmptyList.t) result

  val convert :
    'a t list -> ctxt:Expansion_context.Extension.t -> extension -> 'a option

  val convert_inline_res :
    'a t list ->
    ctxt:Expansion_context.Extension.t ->
    extension ->
    ('a list option, Location.Error.t NonEmptyList.t) result

  val convert_inline :
    'a t list ->
    ctxt:Expansion_context.Extension.t ->
    extension ->
    'a list option
end

val filter_by_context : 'a Context.t -> t list -> 'a For_context.t list
(** Given a context and a list of extension expander, returns all the ones that
    are for this context. *)

module Expert : sig
  (** This module allows to declare extensions that do not produce a value of
      the context type. This is typically useful for extensions point that
      depends on more things from the context than the path and location. *)

  type ('context, 'payload) t
  (** Type of declared expert extensions.

      The ['context] type parameter describes where the extension is expected
      and the ['payload] one what its payload should contain. *)

  val declare :
    string ->
    'context Context.t ->
    (payload, 'a, 'b) Ast_pattern.t ->
    'a ->
    ('context, 'b) t

  val declare_with_path_arg :
    string ->
    'context Context.t ->
    (payload, 'a, 'b) Ast_pattern.t ->
    (arg:Longident.t Loc.t option -> 'a) ->
    ('context, 'b) t

  val convert_res :
    (_, 'a) t list ->
    loc:Location.t ->
    extension ->
    ('a option, Location.Error.t NonEmptyList.t) result

  val convert : (_, 'a) t list -> loc:Location.t -> extension -> 'a option
end

val check_unused : Ast_traverse.iter
val collect_unhandled_extension_errors : Location.Error.t list Ast_traverse.fold

module V2 : sig
  type nonrec t = t

  val declare :
    string ->
    'context Context.t ->
    (payload, 'a, 'context) Ast_pattern.t ->
    (loc:Location.t -> path:string -> 'a) ->
    t

  val declare_inline :
    string ->
    'context Context.t ->
    (payload, 'a, 'context list) Ast_pattern.t ->
    (loc:Location.t -> path:string -> 'a) ->
    t
end

module V3 : sig
  type nonrec t = t

  val declare :
    string ->
    'context Context.t ->
    (payload, 'a, 'context) Ast_pattern.t ->
    (ctxt:Expansion_context.Extension.t -> 'a) ->
    t

  val declare_inline :
    string ->
    'context Context.t ->
    (payload, 'a, 'context list) Ast_pattern.t ->
    (ctxt:Expansion_context.Extension.t -> 'a) ->
    t
end

(**/**)

val check_context_for_inline : func:string -> 'a Context.t -> unit

val __declare_ppx_import :
  string ->
  (ctxt:Expansion_context.Extension.t -> type_declaration -> type_declaration) ->
  t
