(** Convenience functions for feeding the automaton. *)

open! Import

module type Automaton = sig
  module Stack = Automaton_stack

  include module type of struct
    include Automaton_state
  end

  val feed_bytes : (_, 'stack) t -> bytes -> 'stack -> 'stack
  val feed_string : (_, 'stack) t -> string -> 'stack -> 'stack
  val feed_subbytes : (_, 'stack) t -> bytes -> pos:int -> len:int -> 'stack -> 'stack
  val feed_substring : (_, 'stack) t -> string -> pos:int -> len:int -> 'stack -> 'stack
  val feed : ('a, 'b) t -> char -> 'b -> 'b
  val feed_eoi : ('a, 'b) t -> 'b -> 'b
  val old_parser_cont_state : ('a, 'b) t -> Old_parser_cont_state.t

  val of_substring
    :  ('u, 's) Mode.t
    -> ('u, 's) Kind.t
    -> string
    -> pos:int
    -> len:int
    -> ('u, 's) t * 's
end
