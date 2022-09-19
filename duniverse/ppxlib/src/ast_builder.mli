(** Helpers for build OCaml AST fragments *)

open! Import

(** This module is similar to the [Ast_helper] module distributed with OCaml but
    uses different conventions.

    {3 Locations}

    [Ast_helper] uses a global variable for the default locations, we found that
    to it makes it quite easy to mess up locations. Instead this modules forces
    you to provide a location argument.

    For building fragment using the same location everywhere, a functor is
    provided.

    {3 Naming}

    The names match the [Parsetree] names closely, which makes it easy to build
    AST fragments by just knowing the [Parsetree].

    For types of the form a wrapper record with a [_desc] field, helpers are
    generated for each constructor constructing the record directly. For
    instance for the type [Parsetree.expression]:

    {[
      type expression =
        { pexp_desc       : expression_desc
        ; pexp_loc        : Location.t
        ; pexp_attributes : attributes
        }

      and expression_desc =
        | Pexp_ident    of Longident.t loc
        | Pexp_constant of constant
        | Pexp_let      of rec_flag * value_binding list * expression
        ...
    ]}

    The following helpers are created:

    {[
      val pexp_ident    : loc:Location.t -> Longident.t Located.t          -> expression
      val pexp_constant : loc:Location.t -> constant                       -> expression
      val pexp_let      : loc:Location.t -> rec_flag -> value_binding list -> expression
      ...
    ]}

    For other record types, such as type_declaration, we have the following
    helper:

    {[
      type type_declaration =
        { ptype_name       : string Located.t
        ; ptype_params     : (core_type * variance) list
        ; ptype_cstrs      : (core_type * core_type * Location.t) list
        ; ptype_kind       : type_kind
        ; ptype_private    : private_flag
        ; ptype_manifest   : core_type option
        ; ptype_attributes : attributes
        ; ptype_loc        : Location.t
        }


      val type_declaration
        :  loc      : Location.t
        -> name     : string Located.t
        -> params   : (core_type * variance) list
        -> cstrs    : (core_type * core_type * Location.t) list
        -> kind     : type_kind
        -> private  : private_flag
        -> manifest : core_type option
        -> type_declaration
    ]}

    Attributes are always set to the empty list. If you want to set them you
    have to override the field with the [{ e with pexp_attributes = ... }]
    notation. *)

(** Helpers taking a [~loc] argument. This module is meant to be opened or
    aliased. *)
module Default : sig
  module Located :
    Ast_builder_intf.Located
      with type 'a with_loc := 'a Ast_builder_intf.with_location

  include module type of Ast_builder_generated.M

  module Latest : sig
    (** This module contains updated versions of node constructors that were
        kept stable when the node changed. For every function in this module,
        there's an equally-named function outside this module. The function
        outside this module will stay stable, whereas the function inside this
        module will adapt potential upcoming new compiler features. Only use a
        function in this module, if the equally-named one outside this module is
        missing a feature you need. *)

    val ppat_construct :
      loc:location ->
      longident loc ->
      (label loc list * pattern) option ->
      pattern

    val constructor_declaration :
      loc:location ->
      name:label loc ->
      vars:label loc list ->
      args:constructor_arguments ->
      res:core_type option ->
      unit ->
      constructor_declaration
  end

  val ppat_construct :
    loc:location -> longident loc -> pattern option -> pattern

  val constructor_declaration :
    loc:location ->
    name:label loc ->
    args:constructor_arguments ->
    res:core_type option ->
    constructor_declaration

  include
    Ast_builder_intf.Additional_helpers
      with type 'a with_loc := 'a Ast_builder_intf.with_location
end

module type Loc = Ast_builder_intf.Loc

module type S = sig
  include Ast_builder_intf.S

  module Latest : sig
    (** This module contains updated versions of node constructors that were
        kept stable when the node changed. For every function in this module,
        there's an equally-named function outside this module. The function
        outside this module will stay stable, whereas the function inside this
        module will adapt potential upcoming new compiler features. Only use a
        function in this module, if the equally-named one outside this module is
        missing a feature you need. *)

    val ppat_construct :
      longident loc -> (label loc list * pattern) option -> pattern

    val constructor_declaration :
      name:label loc ->
      vars:label loc list ->
      args:constructor_arguments ->
      res:core_type option ->
      unit ->
      constructor_declaration
  end

  val ppat_construct : longident loc -> pattern option -> pattern

  val constructor_declaration :
    name:label loc ->
    args:constructor_arguments ->
    res:core_type option ->
    constructor_declaration
end

(** Build Ast helpers with the location argument factorized. *)
module Make (Loc : Loc) : S

val make : Location.t -> (module S)
(** Functional version of [Make]. *)
