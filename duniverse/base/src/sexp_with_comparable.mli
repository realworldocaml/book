(*_ This module is separated from Sexp to avoid circular dependencies as many things use
  s-expressions *)

(** @inline *)
include module type of struct
  include Sexp
end

include Comparable.S with type t := t
