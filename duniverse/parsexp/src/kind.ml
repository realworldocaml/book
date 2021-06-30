open! Import
include Kind_intf

let create (type stack state) (module Stack : Stack with type t = stack) kind
  : (module S with type Stack.t = stack and type state = state)
  =
  (module struct
    module Stack = Stack

    type nonrec state = state

    let kind = kind
  end)
;;

module Sexp = (val create (module Automaton_stack) Sexp)
module Sexp_with_positions = (val create (module Automaton_stack) Sexp_with_positions)
module Positions = (val create (module Automaton_stack.Just_positions) Positions)
module Cst = (val create (module Automaton_stack.For_cst) Cst)
