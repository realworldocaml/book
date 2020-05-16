(** [Shape.t] are constructed by the [bin_shape] syntax extension from Ocaml type
    definitions & expressions.

    There is a direct mapping from ocaml type definition syntax to the corresponding
    [Shape.group] and from ocaml type expression syntax to the corresponding [Shape.t].
*)
type t [@@deriving sexp_of]

(** [Tid.t] & [Vid.t] are identifiers for type-constructors & type-vars.
    i.e. Given [type 'a t = ... ] *)

module Tid : sig
  (* [t] *)
  type t

  val of_string : string -> t
end

module Vid : sig
  (* ['a] *)
  type t

  val of_string : string -> t
end

(** [Location.t] is required when constructing shapes for which evaluation might fail. *)
module Location : sig
  type t

  val of_string : string -> t
end

(** [Uuid.t] is used by [basetype] and [annotate]. *)
module Uuid : sig
  type t

  (** [of_string s] returns a [Uuid.t] wrapping [s].
      There are currently no requirements of the format of [s] although it is common to
      use string in `uuid' format: XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX
      There is also no attempt to detect & reject duplicates *)
  val of_string : string -> t
end

(** group of mutually recursive type definitions *)
type group

(** This function is generative; repeated calls create distinct groups *)
val group : Location.t -> (Tid.t * Vid.t list * t) list -> group

val tuple : t list -> t
val record : (string * t) list -> t
val variant : (string * t list) list -> t

type poly_variant_row

val constr : string -> t option -> poly_variant_row
val inherit_ : Location.t -> t -> poly_variant_row
val poly_variant : Location.t -> poly_variant_row list -> t

(** recursive apps within the current group *)
val rec_app : Tid.t -> t list -> t

(** apps from outside the group *)
val top_app : group -> Tid.t -> t list -> t

val var : Location.t -> Vid.t -> t

(** Built-in types and types with custom serialization: i.e. int,list,...  To avoid
    accidental protocol compatibility, pass a UUID as the [string] argument *)
val basetype : Uuid.t -> t list -> t

(** [a = annotate s t] creates a shape [a] distinguished, but dependent on shape [t].
    Very much as [record [(s,t)]] does.
    But with [annotate] the ocaml record type does not exist. *)

val annotate : Uuid.t -> t -> t

(** [Shape.Canonical.t] is the result of [eval]uating a shape to a canonical form, and
    represents the shape of Ocaml types w.r.t. bin_io serialization.

    The idea is that de-serialization is safe if the canonical-shape for the type produced
    by de-serialization is equivalent to the canonical-shape of the serialized type.

    The representation is canonical, so equivalence is structural equality.

    [Canonical.t] also provides a useful human level description of a type.

    A [Canonical.t] can be `digested' to a [Digest.t], and except for nearly impossible
    hash collisions, equality of the digests implies equality of canonical-shapes and
    hence equivalence at the Shape.t level.

    [Canonical.t] may also be constructed with various functions:
    [annotate, basetype, tuple, record, variant, poly_variant, fix, recurse, ..]
    which might be used when setting up unit tests or expected shapes. *)

module Digest : sig
  type t [@@deriving compare, sexp]

  val to_hex : t -> string
  val to_md5 : t -> Md5_lib.t
  val of_md5 : Md5_lib.t -> t
end

module Canonical : sig
  type t [@@deriving compare, sexp_of]

  val to_string_hum : t -> string
  val to_digest : t -> Digest.t

  module Exp : sig
    type t
  end

  module Def : sig
    type t
  end

  module Create : sig
    (** [Create.create defs exp] constructs a canonical-shape. The [defs] give context for
        sub-expressions of the form: [apply n exps]; [n] being a reference to the n'th
        definition in [defs].

        Definition are required for [record]s and [variant]s, but may also occurs for any
        cyclic expression: in this case being constructed using [define].

        Within a definition body, [var i] refers to the i'th formal type-var, and
        corresponds to the i'the argument of an application [args]. *)
    val annotate : Uuid.t -> Exp.t -> Exp.t

    val basetype : Uuid.t -> Exp.t list -> Exp.t
    val tuple : Exp.t list -> Exp.t
    val poly_variant : Location.t -> (string * Exp.t option) list -> Exp.t
    val var : int -> Exp.t
    val apply : Def.t -> Exp.t list -> Exp.t
    val recurse : int -> Exp.t list -> Exp.t
    val define : Exp.t -> Def.t
    val record : (string * Exp.t) list -> Exp.t
    val variant : (string * Exp.t list) list -> Exp.t
    val create : Exp.t -> t
  end
end

(** [eval t] returns the canonical-shape for a shape-expression [Shape.t]. Type aliases
    are expanded, so that no [Tid.t] or [Vid.t] have significance in the resulting
    canonical-shape. Type-recursion, including non-regular recursion, is translated to the
    de-bruijn representation used in canonical-shapes. *)
val eval : t -> Canonical.t

(** [eval_to_digest t] returns a hash-value direct from the [Shape.t], potentially
    avoiding the intermediate [Canonical.t] from being constructed. This is important as
    the size of a canonical-shape might be exponential in terms of the size of the shape
    expression.  The following holds:
    [ Digest.(eval_to_digest exp = Canonical.to_digest (eval exp)) ] *)
val eval_to_digest : t -> Digest.t

(** [eval_to_digest_string t] ==  [Digest.to_hex (eval_to_digest t)]
    Convenience function useful for writing unit tests. *)
val eval_to_digest_string : t -> string

module For_typerep : sig
  val deconstruct_tuple_exn : t -> t list
end
