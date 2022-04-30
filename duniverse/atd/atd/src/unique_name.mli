(**
   Functions to translate an identifier into one that's not reserved
   or already taken.

   When necessary, identifiers are modified by adding prefixes or suffixes
   that are compatible with Python syntax.

   Important terminology:
   - source space: ideal set of unique identifiers
   - destination space: set of identifiers that were translated from the
     source space and which guarantees no overlap with a set of reserved
     identifiers.

   The general goal in practice is to minimize the differences between
   source and destination identifiers.

   Things a user might want to do:
   - Given a collection of objects with possibly non-unique names, assign
     a unique identifier to each object, ideally resembling the original name.
     For this, use the [create] function.
   - Given a target language with a set of reserved identifiers, check that
     a name is not a reserved identifier. If it is, modify the name such
     that it's not reserved and is a valid identifier in the target language.
     For this, use the [translate] function.
*)

(** The mutable container holding all translation data. *)
type t

(** Initialize the translation tables by specifying the set of identifiers
    already reserved in the destination space.

    [reserved_identifiers] are forbidden identifiers, i.e. we guarantee
    that a translation will never return one of these.
    [safe_prefix] is a prefix that will be added to an identifier that
    matches one of the reserved prefixes ([reserved_prefixes]).
*)
val init :
  reserved_identifiers: string list ->
  reserved_prefixes: string list ->
  safe_prefix: string ->
  t

(** Reserve a new identifier in the source space. If the given name
    is already an identifier in the source space, a new identifier is
    created by appending an alphanumeric suffix and returned.

    Repeated calls of this function on the same input will produce
    a different output each time.
*)
val create : t -> string -> string

(** Translate an identifier from the source space to the destination space.

    This registers the translation of a name if it's not already registered.
    The translation is a name in the destination space that is not
    reserved for other uses.

    Repeated calls of this function on the same input will produce
    the same output as the previous times.
*)
val translate : t -> string -> string

(** Return whether a name exists in the source space. If it exists, return
    its translation to the destination space. *)
val translate_only : t -> string -> string option

(** Return whether a name exists in the destination space. If it exists,
    return its translation to the source space. *)
val reverse_translate : t -> string -> string option

(** List all the registered identifiers in the source and destination spaces.
    This is a one-to-one mapping sorted alphabetically.
    This is meant for testing and for educational purposes. *)
val all : t -> (string * string) list
