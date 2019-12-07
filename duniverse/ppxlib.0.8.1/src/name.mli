open! Import

module Pattern : sig
  type t

  (** Uses the rules described in [Attribute] *)
  val make : string -> t

  val name : t -> string

  (** [matches ~pattern name] returns [true] iff [name] matches [pattern].

      For instance, the exact set of names such that
      [matches (make "foo.bar.@blah.x") name] is:
      - "foo.bar.blah.x"
      -     "bar.blah.x"
      -         "blah.x"
  *)
  val matches : t -> string -> bool
end

(** Split the path part of a name:

    [split_path "a.b.C.D" = "a.b", Some "C.D"]
*)
val split_path : string -> string * string option

(** [fold_dot_suffixes "foo.@bar.blah" ~init ~f] is

    {[
      ["bar.blah"; "foo.bar.blah"]
    ]}
*)
val dot_suffixes : string -> string list

module Registrar : sig
  (** Names are organized by context. For instance contexts can be: expressions, patterns,
      types, ... *)
  type 'context t

  (** - [kind] is a description of the things registered. For instance: "extension",
      "attribute", ...

      - [current_file] is where this function is called. Must be [__FILE__].

      - [string_of_context]: human readable description of a context
  *)
  val create
    :  kind:string
    -> current_file:string (* must be [__FILE__] *)
    -> string_of_context:('context -> string option)
    -> 'context t

  val register : kind:[ `Attribute | `Extension ] -> 'context t -> 'context -> string -> unit

  val spellcheck :
    'context t -> 'context -> ?white_list:string list -> string -> string option

  val raise_errorf
    :  'context t
    -> 'context
    -> ?white_list:string list
    -> (string -> 'a, unit, string, 'c) format4
    -> string Loc.t
    -> 'a
end

module Whitelisted : sig
  val get_attribute_list : unit -> string list
  val get_extension_list : unit -> string list

  val is_whitelisted : kind:[ `Attribute | `Extension ] -> string -> bool
end

module Reserved_namespaces : sig
  (** [reserve "foo"] has two implications:
        - one can't then declare an attribute inside this namespace
        - attributes within this namespace won't be reported by [check_unused]

      This is here to insure that the rewriter cohabits well with other rewriter
      or tools (e.g. merlin) which might leave attribute on the AST.

      N.B. the "merlin" namespace is reserved by default. *)
  val reserve : string -> unit

  val is_in_reserved_namespaces : string -> bool
end

(** Returns [true] if checks should be ignored for the following name,
    for instance if it is reserved or starts with an underscore. *)
val ignore_checks : string -> bool
