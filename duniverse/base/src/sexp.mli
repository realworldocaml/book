(** Type of S-expressions *)
type t = Sexplib0.Sexp.t =
  | Atom of string
  | List of t list
[@@deriving_inline compare, hash]
include
  sig
    [@@@ocaml.warning "-32"]
    val compare : t -> t -> int
    val hash_fold_t :
      Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
    val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
  end[@@ocaml.doc "@inline"]
[@@@end]

include module type of Sexplib0.Sexp with type t := Sexplib0.Sexp.t

(** Base has never had an [of_string] function.  We expose a deprecated [of_string] here
    so that people can find it (e.g. with merlin), and learn what we recommend.  This
    [of_string] has type [unit] because we don't want it to be accidentally used. *)
val of_string : unit
[@@deprecated "[since 2018-02] Use [Parsexp.Single.parse_string_exn]"]
