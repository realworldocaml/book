(** Prefixes of valid sexps, terminating in a hole where additional input is expected to
    complete the sexp.  *)

open Import
open Ppx_sexp_conv_lib

module type Sexp_prefix = sig
  (** The outermost constructor represents the outermost sexp. (This is the opposite of
      [Automaton.Stack.t], where the outermost constructor is the innermost sexp.)

      [Hole] also encodes the information about the incomplete atom, which is otherwise
      stored in [State.t]. *)
  type t = Sexp.t list * in_sexp

  and in_sexp =
    | Hole of Atom_prefix.t option
    | In_list of t
  [@@deriving sexp_of]

  (** [create state] returns [Some t] if the parser is known to be in a sexp.

      "In a sexp" does not include comments, even comments nested inside sexps.

      [create] returns [None] when the parser is uncertain whether or not it is in sexp.
      For example, [#] not in double-quotes can be either an atom or a block comment. The
      parser must consume the next character to determine which.

      The automaton must be in [Many] mode. *)
  val create
    :  (Positions.Builder.t, Automaton.Stack.t) Automaton.t
    -> Automaton.Stack.t
    -> t option

  val of_substring : string -> pos:int -> len:int -> t option

  (** [get_a_signifier t ~parser_input] returns a string which, when prepended to the
      unconsumed parser input, will parse to the same sexp(s) as the original parser
      input.

      Unlike [Atom_prefix.get_signifier], [get_a_signifier] does not promise to return the
      original signifier of this prefix in the parser input. *)
  val get_a_signifier : t -> parser_input:string -> string
end
