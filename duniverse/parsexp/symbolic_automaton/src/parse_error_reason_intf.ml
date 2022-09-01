open! Base

(** Subset of the [Parsexp.Parse_error.Reason.t] type **)

module T = struct
  type t =
    | Automaton_in_error_state
    | Comment_token_in_unquoted_atom
    | Unexpected_char_parsing_dec_escape
    | Unexpected_char_parsing_hex_escape
    | Unexpected_character_after_cr
    | Unterminated_block_comment
    | Unterminated_quoted_string
  [@@deriving compare, enumerate, hash, sexp_of, variants]
end

module type Parse_error_reason = sig
  include module type of struct
    include T
  end

  val to_string : t -> string
end
