open! Import
include Parse_error_intf

type t =
  { position : Positions.pos
  ; message : string
  ; old_parser_exn : [ `Parse_error | `Failure ]
  }

let sexp_of_t { position; message; old_parser_exn = _ } : Sexp.t =
  List
    [ List [ Atom "position"; Positions.sexp_of_pos position ]
    ; List [ Atom "message"; sexp_of_string message ]
    ]
;;

let position t = t.position
let message t = t.message
let old_parser_exn t = t.old_parser_exn

let report ppf ~filename t =
  let pos = position t in
  let msg = message t in
  Format.fprintf
    ppf
    "File \"%s\", line %d, character %d:\nError: s-expression parsing error;\n%s\n"
    filename
    pos.line
    pos.col
    msg
;;

exception Parse_error of t [@@deriving_inline sexp]

let () =
  Ppx_sexp_conv_lib.Conv.Exn_converter.add [%extension_constructor Parse_error] (function
    | Parse_error v0 ->
      let v0 = sexp_of_t v0 in
      Ppx_sexp_conv_lib.Sexp.List
        [ Ppx_sexp_conv_lib.Sexp.Atom "parse_error.ml.Parse_error"; v0 ]
    | _ -> assert false)
;;

[@@@end]

let raise (reason : Reason.t) position ~at_eof ~atom_buffer =
  let message =
    (* These messages where choosen such that we can build the various Sexplib parsing
       functions on top of Parsexp and keep the same exceptions.

       At the time of writing this, a simple layer on top of parsexp to implement the
       sexplib API is passing all the sexplib tests.

       Note that parsexp matches the semantic of Sexp.parse which is slightly
       different from the ocamllex/ocamlyacc based parser of Sexplib. The latter one
       is less tested and assumed to be less used. *)
    match reason with
    | Unexpected_char_parsing_hex_escape -> "unterminated hexadecimal escape sequence"
    | Unexpected_char_parsing_dec_escape -> "unterminated decimal escape sequence"
    | Unterminated_quoted_string -> "unterminated quoted string"
    | Unterminated_block_comment -> "unterminated block comment"
    | Escape_sequence_out_of_range -> "escape sequence in quoted string out of range"
    | Unclosed_paren -> "unclosed parentheses at end of input"
    | Too_many_sexps -> "s-expression followed by data"
    | Closed_paren_without_opened -> "unexpected character: ')'"
    | Comment_token_in_unquoted_atom ->
      if String.equal (Buffer.contents atom_buffer) "|"
      then "illegal end of comment"
      else "comment tokens in unquoted atom"
    | Sexp_comment_without_sexp -> "unterminated sexp comment"
    | Unexpected_character_after_cr ->
      if at_eof
      then "unexpected end of input after carriage return"
      else "unexpected character after carriage return"
    | No_sexp_found_in_input -> "no s-expression found in input"
    | Automaton_in_error_state -> failwith "Parsexp.Parser_automaton: parser is dead"
  in
  let old_parser_exn =
    match reason, at_eof with
    | Too_many_sexps, _ | _, true -> `Failure
    | Comment_token_in_unquoted_atom, _
      when String.equal (Buffer.contents atom_buffer) "|" -> `Failure
    | _ -> `Parse_error
  in
  raise (Parse_error { position; message; old_parser_exn })
;;

module Private = struct
  module Reason = Reason

  let old_parser_exn = old_parser_exn
  let raise = raise
end
