open! Base
include Table_intf

type t =
  { transitions : Transition.t Or_parse_error_reason.t array
  ; transitions_eoi : Automaton.Epsilon_action.t list Or_parse_error_reason.t array
  }
[@@deriving sexp_of]

let advance : char -> Advance.t = function
  | '\n' -> Advance_eol
  | _ -> Advance
;;

let compile (module A : Automaton.S) : t =
  let rec squash acc state c : Transition.t Or_parse_error_reason.t =
    match A.transition (state, c) with
    | T (action, state) ->
      Ok
        { action = List.rev acc, action
        ; goto = State (State.to_int state)
        ; advance = advance c
        }
    | E (action, state) -> squash (action :: acc) state c
    | Error error -> Error error
    | End_block_comment ->
      Ok { action = List.rev acc, Nop; goto = End_block_comment; advance = advance c }
  in
  let rec squash_eoi acc state : Automaton.Epsilon_action.t list Or_parse_error_reason.t =
    match A.transition_eoi state with
    | Eoi_check -> Ok (List.rev acc)
    | E (eps_action, state) -> squash_eoi (eps_action :: acc) state
    | Error error -> Error error
  in
  let transitions =
    let dummy_transition : Transition.t =
      { action = [], Nop; goto = State 0; advance = Advance }
    in
    Array.create
      ~len:(State.count * 256)
      (Ok dummy_transition : _ Or_parse_error_reason.t)
  in
  let transitions_eoi =
    Array.create (Ok [] : _ Or_parse_error_reason.t) ~len:State.count
  in
  for s = 0 to State.count - 1 do
    let state = State.of_int s in
    for c = 0 to 255 do
      transitions.((s * 256) + c) <- squash [] state (Char.of_int_exn c)
    done;
    transitions_eoi.(s) <- squash_eoi [] state
  done;
  { transitions; transitions_eoi }
;;
