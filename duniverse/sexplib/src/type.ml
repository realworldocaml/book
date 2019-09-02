(** Type of S-expressions *)
type t = Sexplib0.Sexp.t = Atom of string | List of t list
