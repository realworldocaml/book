open! Import

type t =
  | Parsing_toplevel_whitespace
  | Parsing_nested_whitespace
  | Parsing_atom
  | Parsing_list
  | Parsing_sexp_comment
  | Parsing_block_comment
[@@deriving_inline sexp_of]

include sig
  [@@@ocaml.warning "-32"]

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end
[@@ocaml.doc "@inline"]

[@@@end]

val to_string : t -> string
