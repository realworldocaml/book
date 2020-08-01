module Sexp : sig
  type t =
  | Atom of string
  | List of t list
end
