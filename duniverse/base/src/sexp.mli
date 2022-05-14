(** Type of S-expressions *)
type t = Sexplib0.Sexp.t =
  | Atom of string
  | List of t list
[@@deriving_inline hash]

include Ppx_hash_lib.Hashable.S with type t := t

[@@@end]

include module type of Sexplib0.Sexp with type t := Sexplib0.Sexp.t

val t_sexp_grammar : t Sexplib0.Sexp_grammar.t

val invariant : t -> unit

(** Base has never had an [of_string] function.  We expose a deprecated [of_string] here
    so that people can find it (e.g. with merlin), and learn what we recommend.  This
    [of_string] has type [unit] because we don't want it to be accidentally used. *)
val of_string : unit
[@@deprecated "[since 2018-02] Use [Parsexp.Single.parse_string_exn]"]
