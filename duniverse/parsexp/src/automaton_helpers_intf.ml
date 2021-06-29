(** Convenience functions for feeding the automaton. *)

open! Import

module type Automaton_helpers = sig
  val feed_bytes : (_, 'stack) Parser_automaton.state -> bytes -> 'stack -> 'stack
  val feed_string : (_, 'stack) Parser_automaton.state -> string -> 'stack -> 'stack

  val feed_subbytes
    :  (_, 'stack) Parser_automaton.state
    -> bytes
    -> pos:int
    -> len:int
    -> 'stack
    -> 'stack

  val feed_substring
    :  (_, 'stack) Parser_automaton.state
    -> string
    -> pos:int
    -> len:int
    -> 'stack
    -> 'stack
end
