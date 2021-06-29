(** Stable printing functions built on top of compiler-libs types
Use this for user facing part of the code so that mdx's output does not
depend on the ocaml version it was built with. *)

module Location : sig
  val print_loc : Format.formatter -> Location.t -> unit
  (** Prints location for error reporting as ["File <file>, lines <line-range>"] *)
end
