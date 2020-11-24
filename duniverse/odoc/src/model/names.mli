(** Typed names for paths, identifiers, references and fragments.

    This module contains a module per type of named object in our internal
    representation of the langage, each containing an opaque type [t].
    This allows us to ensure that, for example, we never mistake a module
    name for a module type name.
*)

(** Name is the signature for names that could possibly be internal. Internal
    names occur when we generate items that don't have a path that will be
    exposed in the generated HTML, for example, when we are doing generalised
    opens. The compiler makes sure these new types are removed from the
    signature, so they should never be externally visible, and an attempt to
    turn an internal name into a string will result in an exception being thrown.

    Note that it is tricky currently to remove references to internal names,
    and hence the 'safe' [to_string] will not currently raise an exception. When
    the model is updated to handle this the exception will be reinstated. *)
module type Name = sig

    type t

    val to_string : t -> string

    (** [to_string_unsafe] will allow even internal names to be turned into
        strings. Use with caution. *)
    val to_string_unsafe : t -> string

    val of_string : string -> t

    val of_ident : Ident.t -> t

    val internal_of_string : string -> t

    val internal_of_ident : Ident.t -> t

    val is_internal : t -> bool

    val equal : t -> t -> bool

    (** Hidden names are those that contain a double underscore, e.g.
        [Hidden__module] *)
    val is_hidden : t -> bool
end

(** Some named objects can't have internal names, so they have this simpler
    module. *)
module type SimpleName = sig

    type t

    val to_string : t -> string

    val of_string : string -> t

    val of_ident : Ident.t -> t

    val equal : t -> t -> bool

    val is_hidden : t -> bool

end

module ModuleName : Name

module ArgumentName : Name

module ModuleTypeName : Name

module TypeName : Name

module ConstructorName : SimpleName

module FieldName : SimpleName

module ExtensionName : SimpleName

module ExceptionName : SimpleName

module ValueName : SimpleName

module ClassName : Name

module ClassTypeName : Name

module MethodName : SimpleName

module InstanceVariableName : SimpleName

module UnitName : SimpleName

module LabelName : SimpleName

module PageName : SimpleName
