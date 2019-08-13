(** Representation of parsed [%expect] lines *)

type t =
  | Regexp  of string
  | Glob    of string
  | Literal of string
[@@deriving sexp_of, compare]
