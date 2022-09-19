(** Parser automaton *)

type u'
type s'

val transitions : Automaton_action.Poly.t array
val transitions_eoi : Automaton_action.Epsilon.Poly.t array
val old_parser_approx_cont_states : Old_parser_cont_state.t array
