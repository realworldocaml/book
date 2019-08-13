(** Parser automaton *)

(** Warning: the API of this module is not fixed and might change! Use {!Sexp_parsing} for
    the stable version. *)

include module type of Parser_automaton_internal.Public

val feed     : ('u, 's) state -> char -> 's -> 's
val feed_eoi : ('u, 's) state ->         's -> 's

(**/**)

(*_ Continuation state of the old parser  *)
val old_parser_cont_state : _ state -> Old_parser_cont_state.t
