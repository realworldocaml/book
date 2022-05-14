(** Compile [Automaton] to transition table. *)

open! Base

module Action = struct
  type t = Automaton.Epsilon_action.t list * Automaton.Action.t
  [@@deriving compare, sexp_of, hash]
end

module Goto_state = struct
  type t =
    | State of int
    | End_block_comment
  [@@deriving compare, sexp_of, hash]
end

module Advance = struct
  type t =
    | Advance
    | Advance_eol
  [@@deriving compare, sexp_of, hash]
end

module Transition = struct
  type t =
    { action : Action.t
    ; goto : Goto_state.t
    ; advance : Advance.t
    }
  [@@deriving compare, sexp_of, hash]
end

module Or_parse_error_reason = struct
  type 'a t =
    | Ok of 'a
    | Error of Parse_error_reason.t
  [@@deriving compare, sexp_of, hash]
end

module type Table = sig
  module Action = Action
  module Goto_state = Goto_state
  module Advance = Advance
  module Transition = Transition
  module Or_parse_error_reason = Or_parse_error_reason

  type t =
    { transitions : Transition.t Or_parse_error_reason.t array
    ; transitions_eoi : Automaton.Epsilon_action.t list Or_parse_error_reason.t array
    }
  [@@deriving sexp_of]

  val compile : (module Automaton.S) -> t
end
