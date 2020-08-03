(** One typically obtains a [Source_code_position.t] using a [[%here]] expression, which
    is implemented by the [ppx_here] preprocessor. *)

open! Import

(** See INRIA's OCaml documentation for a description of these fields.

    [sexp_of_t] uses the form ["FILE:LINE:COL"], and does not have a corresponding
    [of_sexp]. *)
type t = Caml.Lexing.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving_inline hash, sexp_of]

val hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

[@@@end]

include Comparable.S with type t := t

(** [to_string t] converts [t] to the form ["FILE:LINE:COL"]. *)
val to_string : t -> string

(** [of_pos Caml.__POS__] is like [[%here]] but without using ppx. *)
val of_pos : string * int * int * int -> t

