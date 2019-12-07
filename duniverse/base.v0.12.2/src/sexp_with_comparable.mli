(*_ This module is separated from Sexp to avoid circular dependencies as many things use
  s-expressions *)

include module type of struct include Sexp end (** @inline *)

include Comparable.S with type t := t
