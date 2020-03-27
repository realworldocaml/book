open! Import

module Non_intersecting_ranges : sig
  type t

  val empty : t
end

val enforce_invariants : string option -> Non_intersecting_ranges.t Ast_traverse.fold
