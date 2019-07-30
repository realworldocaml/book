(** ATD annotations relating to OCaml validators. *)

type validate_repr = (string option * bool)

val get_validator : Atd.Annot.t -> string option
