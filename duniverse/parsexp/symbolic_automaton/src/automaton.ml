open! Base
include Automaton_intf
module State = State

module Quoted_string_transition = struct
  type t =
    | T of Action.t * State.Quoted_string.t
    | E of Epsilon_action.t * State.Quoted_string.t
    | Error of Parse_error_reason.t
    | End_of_quoted_string
end

type context =
  | In_block_comment
  | In_atom

let quoted_string_transition
  : context -> State.Quoted_string.t * char -> Quoted_string_transition.t
  =
  fun context x ->
  (* Distinguishing atom and block comments is to optimize block comments. But
     we musn't optimize the exception on things like \321. *)
  let if_atom then_ else_ : Action.t =
    match context with
    | In_atom -> then_
    | In_block_comment -> else_
  in
  let if_atom_eps then_ else_ : Epsilon_action.t =
    match context with
    | In_atom -> then_
    | In_block_comment -> else_
  in
  match x with
  | Normal, '"' -> End_of_quoted_string
  | Normal, '\\' -> T (Add_token_char, After_backslash)
  | Normal, _ -> T (if_atom Add_quoted_atom_char Add_token_char, Normal)
  | After_backslash, '\n' -> T (Add_token_char, Ignoring_blanks)
  | After_backslash, '\r' -> T (Add_token_char, After_backslash_cr)
  | After_backslash, 'x' -> T (Add_token_char, After_backslash_x)
  | After_backslash, '0' .. '9' -> T (Add_dec_escape_char, After_backslash_digit)
  | After_backslash, _ -> T (if_atom Add_escaped Add_token_char, Normal)
  | After_backslash_cr, '\n' -> T (Add_token_char, Ignoring_blanks)
  | After_backslash_cr, _ -> E (if_atom_eps Add_escaped_cr Nop, Normal)
  | After_backslash_x, ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') ->
    T (if_atom Add_hex_escape_char Add_token_char, After_backslash_x_hex)
  | After_backslash_x, _ -> Error Unexpected_char_parsing_hex_escape
  | After_backslash_x_hex, ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F') ->
    T (if_atom Add_last_hex_escape_char Add_token_char, Normal)
  | After_backslash_x_hex, _ -> Error Unexpected_char_parsing_hex_escape
  | After_backslash_digit, '0' .. '9' -> T (Add_dec_escape_char, After_backslash_2digits)
  | After_backslash_digit, _ -> Error Unexpected_char_parsing_dec_escape
  | After_backslash_2digits, '0' .. '9' ->
    T (if_atom Add_last_dec_escape_char Comment_add_last_dec_escape_char, Normal)
  | After_backslash_2digits, _ -> Error Unexpected_char_parsing_dec_escape
  | Ignoring_blanks, (' ' | '\t') -> T (Add_token_char, Ignoring_blanks)
  | Ignoring_blanks, _ -> E (Nop, Normal)
;;

module Block_comment_transition = struct
  type t =
    | T of Action.t * State.Block_comment.t
    | E of Epsilon_action.t * State.Block_comment.t
    | Error of Parse_error_reason.t
    | End_comment
end

let block_comment_transition : State.Block_comment.t * char -> Block_comment_transition.t
  = function
    | Quoted_string state, c ->
      (match quoted_string_transition In_block_comment (state, c) with
       | End_of_quoted_string -> T (Add_token_char, Normal)
       | T (action, state) -> T (action, Quoted_string state)
       | E (action, state) -> E (action, Quoted_string state)
       | Error error -> Error error)
    | After_hash, '|' -> T (Start_block_comment, Normal)
    | After_pipe, '#' -> End_comment
    | _, '"' -> T (Add_token_char, Quoted_string Normal)
    | _, '|' -> T (Add_token_char, After_pipe)
    | _, '#' -> T (Add_token_char, After_hash)
    | _, _ -> T (Add_token_char, Normal)
;;

let transition : State.t * char -> Transition.t = function
  | Whitespace, '(' -> T (Opening, Whitespace)
  | Whitespace, ')' -> T (Closing, Whitespace)
  | Whitespace, '\r' -> T (Nop, After_cr)
  | Whitespace, (' ' | '\t' | '\012' | '\n') -> T (Nop, Whitespace)
  | Whitespace, ';' -> T (Start_line_comment, Line_comment)
  | Whitespace, '"' -> T (Start_quoted_string, Quoted_string Normal)
  | Whitespace, '#' -> T (Nop, After_hash)
  | Whitespace, '|' -> T (Add_first_char, Unquoted_string After_pipe)
  | Whitespace, _ -> T (Add_first_char, Unquoted_string Normal)
  | After_cr, '\n' -> T (Nop, Whitespace)
  | After_cr, _ -> Error Unexpected_character_after_cr
  | Unquoted_string _, (';' | '(' | ')' | '"' | ' ' | '\t' | '\012' | '\r' | '\n') ->
    E (Push_atom, Whitespace)
  | Unquoted_string After_hash, '|' | Unquoted_string After_pipe, '#' ->
    Error Comment_token_in_unquoted_atom
  | Unquoted_string _, '#' -> T (Add_atom_char, Unquoted_string After_hash)
  | Unquoted_string _, '|' -> T (Add_atom_char, Unquoted_string After_pipe)
  | Unquoted_string _, _ -> T (Add_atom_char, Unquoted_string Normal)
  | Line_comment, ('\r' | '\n') -> E (End_line_comment, Whitespace)
  | Line_comment, _ -> T (Add_token_char, Line_comment)
  | After_hash, ';' -> T (Start_sexp_comment, Whitespace)
  | After_hash, '|' -> T (Start_block_comment, Block_comment Normal)
  | After_hash, _ -> E (Add_first_char_hash, Unquoted_string Normal)
  | Quoted_string state, c ->
    (match quoted_string_transition In_atom (state, c) with
     | End_of_quoted_string -> T (Push_quoted_atom, Whitespace)
     | T (action, state) -> T (action, Quoted_string state)
     | E (action, state) -> E (action, Quoted_string state)
     | Error error -> Error error)
  | Block_comment state, c ->
    (match block_comment_transition (state, c) with
     | T (action, state) -> T (action, Block_comment state)
     | E (action, state) -> E (action, Block_comment state)
     | End_comment -> End_block_comment
     | Error error -> Error error)
  | Error, _ -> Error Automaton_in_error_state
;;

let transition_eoi : State.t -> Final_transition.t = function
  | Whitespace -> Eoi_check
  | After_cr -> Error Unexpected_character_after_cr
  | Unquoted_string _ -> E (Push_atom, Whitespace)
  | Line_comment -> E (End_line_comment, Whitespace)
  | After_hash -> E (Add_first_char_hash, Unquoted_string Normal)
  | Quoted_string _ -> Error Unterminated_quoted_string
  | Block_comment _ -> Error Unterminated_block_comment
  | Error -> Error Automaton_in_error_state
;;

let action_to_runtime_function : Action.t -> string option = function
  | Nop -> None
  | t -> Some (String.uncapitalize (Action.Variants.to_name t))
;;

let epsilon_action_to_runtime_function : Epsilon_action.t -> string option = function
  | Nop -> None
  | End_line_comment -> Some "end_line_comment"
  | t -> Some ("eps_" ^ String.uncapitalize (Epsilon_action.Variants.to_name t))
;;
