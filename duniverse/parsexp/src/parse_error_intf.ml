open! Import

module Reason = struct
  (*_ To be kept in sync with the Error module in gen/gen_parser_automaton.ml *)
  type t =
    | Unexpected_char_parsing_hex_escape
    | Unexpected_char_parsing_dec_escape
    | Unterminated_quoted_string
    | Unterminated_block_comment
    | Escape_sequence_out_of_range
    | Unclosed_paren
    | Too_many_sexps
    | Closed_paren_without_opened
    | Comment_token_in_unquoted_atom
    | Sexp_comment_without_sexp
    | Unexpected_character_after_cr
    | No_sexp_found_in_input
    | Automaton_in_error_state
end

module type Parse_error = sig
  type t [@@deriving_inline sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val position : t -> Positions.pos
  val message : t -> string

  (** Report an error in a style similar to OCaml, for instance:

      File "blah", line 42, character 10:
      Error: s-expression parsing error;
      unterminated quoted string.
  *)
  val report : Format.formatter -> filename:string -> t -> unit

  exception Parse_error of t

  (**/**)

  module Private : sig
    module Reason = Reason

    (** To match the old behavior, the old parser sometimes raised [Failure] and sometimes
        raised [Parse_error] *)
    val old_parser_exn : t -> [ `Parse_error | `Failure ]

    val raise : Reason.t -> Positions.pos -> at_eof:bool -> atom_buffer:Buffer.t -> 'a
  end
end
