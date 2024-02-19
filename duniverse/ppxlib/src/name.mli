open! Import

module Pattern : sig
  type t

  val make : string -> t
  (** Uses the rules described in [Attribute] *)

  val name : t -> string

  val matches : t -> string -> bool
  (** [matches ~pattern name] returns [true] iff [name] matches [pattern].

      For instance, the exact set of names such that
      [matches (make "foo.bar.@blah.x") name] is:

      - "foo.bar.blah.x"
      - "bar.blah.x"
      - "blah.x" *)
end

val split_path : string -> string * string option
(** Split the path part of a name:

    [split_path "a.b.C.D" = "a.b", Some "C.D"] *)

val dot_suffixes : string -> string list
(** [fold_dot_suffixes "foo.@bar.blah" ~init ~f] is

    {[
      [ "bar.blah"; "foo.bar.blah" ]
    ]} *)

module Registrar : sig
  type 'context t
  (** Names are organized by context. For instance contexts can be: expressions,
      patterns, types, ... *)

  val create :
    kind:string ->
    current_file:string (* must be [__FILE__] *) ->
    string_of_context:('context -> string option) ->
    'context t
  (** - [kind] is a description of the things registered. For instance:
        "extension", "attribute", ...

      - [current_file] is where this function is called. Must be [__FILE__].

      - [string_of_context]: human readable description of a context *)

  val register :
    kind:[ `Attribute | `Extension ] -> 'context t -> 'context -> string -> unit

  val check_collisions : 'context t -> 'context -> string -> unit

  val spellcheck :
    'context t -> 'context -> ?allowlist:string list -> string -> string option

  module Error : sig
    val createf :
      'context t ->
      'context ->
      ?allowlist:string list ->
      (string -> Location.Error.t, unit, string, Location.Error.t) format4 ->
      string Loc.t ->
      Location.Error.t

    val raise_errorf :
      'context t ->
      'context ->
      ?allowlist:string list ->
      (string -> Location.Error.t, unit, string, Location.Error.t) format4 ->
      string Loc.t ->
      'a

    val error_extensionf :
      'context t ->
      'context ->
      ?allowlist:string list ->
      (string -> Location.Error.t, unit, string, Location.Error.t) format4 ->
      string Loc.t ->
      extension
  end

  val raise_errorf :
    'context t ->
    'context ->
    ?allowlist:string list ->
    (string -> Location.Error.t, unit, string, Location.Error.t) format4 ->
    string Loc.t ->
    'a
end

module Allowlisted : sig
  val get_attribute_list : unit -> string list
  val get_extension_list : unit -> string list
  val is_allowlisted : kind:[ `Attribute | `Extension ] -> string -> bool
end

module Reserved_namespaces : sig
  val reserve : string -> unit
  (** [reserve "foo"] has two implications:

      - one can't then declare an attribute inside this namespace
      - attributes within this namespace won't be reported by [check_unused]

      This is here to insure that the rewriter cohabits well with other rewriter
      or tools (e.g. merlin) which might leave attribute on the AST.

      N.B. the "merlin" namespace is reserved by default. *)

  val is_in_reserved_namespaces : string -> bool
end

val ignore_checks : string -> bool
(** Returns [true] if checks should be ignored for the following name, for
    instance if it is reserved or starts with an underscore. *)
