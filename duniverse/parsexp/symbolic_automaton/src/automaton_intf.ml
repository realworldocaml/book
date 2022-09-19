open! Base

(** Action associated to transitions. Actions correspond to the similarly named functions
    in [Parsexp.Automaton_actions]. **)
module Action = struct
  type t =
    | Nop
    | Opening
    | Closing
    | Add_atom_char
    | Add_quoted_atom_char
    | Add_first_char
    | Add_escaped
    | Add_hex_escape_char
    | Add_dec_escape_char
    | Add_last_hex_escape_char
    | Add_last_dec_escape_char
    | Add_token_char
    | Comment_add_last_dec_escape_char
    | Push_quoted_atom
    | Start_quoted_string
    | Start_block_comment
    | Start_sexp_comment
    | Start_line_comment
  [@@deriving compare, sexp_of, hash, variants]
end

(** Action associated to epsilon transitions, i.e. transitions that do not consume a
    character.

    Actions correspond to the similarly named functions in [Parsexp.Automaton_actions].

    Having epsilon actions makes the definition of the automaton much simpler. **)
module Epsilon_action = struct
  type t =
    | Nop
    | Push_atom
    | Add_first_char_hash
    | Add_escaped_cr
    | End_line_comment
  [@@deriving compare, sexp_of, hash, variants]
end

module Transition = struct
  type t =
    | T of Action.t * State.t
    | E of Epsilon_action.t * State.t
    | Error of Parse_error_reason.t
    | End_block_comment
    (* can't be a normal transition, as the new state isn't known
       statically *)
  [@@deriving compare]
end

module Final_transition = struct
  type t =
    | Eoi_check
    | E of Epsilon_action.t * State.t
    | Error of Parse_error_reason.t
end

module type S = sig
  val transition : State.t * char -> Transition.t
  val transition_eoi : State.t -> Final_transition.t
end

module type Automaton = sig
  module Action = Action
  module Epsilon_action = Epsilon_action
  module Transition = Transition
  module Final_transition = Final_transition
  module State = State

  module type S = S

  include S

  val action_to_runtime_function : Action.t -> string option
  val epsilon_action_to_runtime_function : Epsilon_action.t -> string option
end
