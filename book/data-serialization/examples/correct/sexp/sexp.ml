module Sexp = struct
  type t =
  | Atom of string
  | List of t list
end
