open! Import

module type S = sig
  type ast
  type t

  val create_list : ast list -> t list

  (** Add to hash state via [hash_fold_int] to signify what clause we're in. [None] for
      inherited polymorphic variant clauses, since their observers will have salt for
      concrete tags. *)
  val salt : t -> int option

  (** location of the clause's definition *)
  val location : t -> location

  (** weight of the clause relative to other clauses in the generator distribution *)
  val weight : t -> expression

  (** types of the clause's arguments *)
  val core_type_list : t -> core_type list

  (** constructing a pattern to match the clause *)
  val pattern : t -> loc:location -> pattern list -> pattern

  (** constructing an expression to create an instance of the clause *)
  val expression : t -> loc:location -> core_type -> expression list -> expression
end

module type Clause_syntax = sig
  module type S = S

  module Variant : S with type ast = constructor_declaration
  module Polymorphic_variant : S with type ast = row_field
end
