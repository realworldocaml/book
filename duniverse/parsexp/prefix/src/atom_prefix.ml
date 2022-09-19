open! Ppx_compare_lib.Builtin
open! Import
include Atom_prefix_intf

type t =
  { signified : Signified.t
  ; signifier_begin_offset : int
  ; signifier_end_offset : int
  }
[@@deriving compare, sexp_of]

let offset_of_start_of_current_quoted_atom position_builder =
  match Positions.Builder.contents position_builder |> Positions.to_list |> List.rev with
  | [] -> failwith "BUG: No positions saved even though we have begun a quoted atom."
  | { offset; line = _; col = _ } :: _ -> offset
;;

let incomplete prefix_of_prefix : Signified.t = Incomplete { prefix_of_prefix }
let complete prefix : Signified.t = Complete { prefix }

let create_unquoted (state : (_, _) Automaton.t) make_signified =
  let prefix = Buffer.contents state.atom_buffer in
  { signified = make_signified prefix
  ; signifier_begin_offset = state.offset - String.length prefix
  ; signifier_end_offset = state.offset
  }
;;

let create_quoted (state : (_, _) Automaton.t) make_signified =
  let prefix = Buffer.contents state.atom_buffer in
  { signified = make_signified prefix
  ; signifier_begin_offset = offset_of_start_of_current_quoted_atom state.user_state
  ; signifier_end_offset = state.offset
  }
;;

let create (state : (_, _) Automaton.t) : t Create_result.t =
  match Automaton.context state with
  | Sexp_comment -> Awkward_position
  | Sexp ->
    (match Parsexp_symbolic_automaton.Automaton.State.of_int state.automaton_state with
     | Whitespace -> Whitespace
     | Error | After_cr | Line_comment | After_hash | Block_comment _ -> Awkward_position
     | Unquoted_string _ -> Some (create_unquoted state complete)
     | Quoted_string (Normal | Ignoring_blanks) -> Some (create_quoted state complete)
     | Quoted_string
         ( After_backslash
         | After_backslash_cr
         | After_backslash_digit
         | After_backslash_2digits
         | After_backslash_x
         | After_backslash_x_hex ) -> Some (create_quoted state incomplete))
;;

let create_opt state =
  match create state with
  | Some t -> Some t
  | Awkward_position | Whitespace -> None
;;

let get_signified t = t.signified
let get_signifier_length t = t.signifier_end_offset - t.signifier_begin_offset

let get_signifier t ~parser_input =
  String.sub parser_input t.signifier_begin_offset (get_signifier_length t)
;;

module Unstable = struct
  type nonrec t = t =
    { signified : Signified.Unstable.t
    ; signifier_begin_offset : int
    ; signifier_end_offset : int
    }
  [@@deriving sexp]
end
