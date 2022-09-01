open! Import

module Reason = struct
  (*_ Some of these come from [Parsexp_symbolic_automaton.Parse_error_reason]. *)
  type t =
    (*$ Parsexp_cinaps_helpers.Gen_parse_error.print_constructors () *)
    | Automaton_in_error_state
    | Comment_token_in_unquoted_atom
    | Unexpected_char_parsing_dec_escape
    | Unexpected_char_parsing_hex_escape
    | Unexpected_character_after_cr
    | Unterminated_block_comment
    | Unterminated_quoted_string
    (*$*)
    | Closed_paren_without_opened
    | Escape_sequence_out_of_range
    | No_sexp_found_in_input
    | Sexp_comment_without_sexp
    | Too_many_sexps
    | Unclosed_paren
end

module type Parse_error = sig
  type t [@@deriving_inline sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val sexp_of_t : t -> Sexplib0.Sexp.t
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
