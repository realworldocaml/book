(** Module for the type [unit]. *)

open! Import

type t = unit [@@deriving_inline compare, enumerate, hash, sexp]
include
  sig
    [@@@ocaml.warning "-32"]
    val compare : t -> t -> int
    val all : t list
    val hash_fold_t :
      Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
    val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
    include Ppx_sexp_conv_lib.Sexpable.S with type  t :=  t
  end[@@ocaml.doc "@inline"]
[@@@end]

include Identifiable.S with type t := t
include Invariant.S with type t := t
