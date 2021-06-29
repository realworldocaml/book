open! Import

module type Stack = sig
  (** Parser stack. The stack is not in [state] for optimization purposes. *)
  type t

  val empty : t
end

module type S = sig
  module Stack : Stack

  type state

  val kind : (state, Stack.t) Parser_automaton_internal.Public.kind
end

module type Kind = sig
  module type Stack = Stack
  module type S = S

  module Sexp : S with type Stack.t = Automaton_stack.t and type state = unit

  module Sexp_with_positions :
    S with type Stack.t = Automaton_stack.t and type state = Positions.Builder.t

  module Positions :
    S
    with type Stack.t = Automaton_stack.Just_positions.t
     and type state = Positions.Builder.t

  module Cst :
    S
    with type Stack.t = Automaton_stack.For_cst.t
     and type state = Parser_automaton_internal.Public.state_cst
end
