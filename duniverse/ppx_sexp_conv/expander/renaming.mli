(* A renaming is a mapping from type variable name to type variable name.
   In definitions such as:

   type 'a t =
   | A : <type> -> 'b t
   | B of 'a

   we generate a function that takes an sexp_of parameter named after 'a, but 'a is not in
   scope in <type> when handling the constructor A (because A is a gadt constructor).
   Instead the type variables in scope are the ones defined in the return type of A,
   namely 'b. There could be less or more type variable in cases such as:

   type _ less = Less : int less
   type _ more = More : ('a * 'a) more

   If for instance, <type> is ['b * 'c], when we find 'b, we will look for ['b] in the
   renaming and find ['a] (only in that gadt branch, it could be something else in other
   branches), at which point we can call the previously bound sexp_of parameter named
   after 'a.
   If we can't find a resulting name, like when looking up ['c] in the renaming, then we
   assume the variable is existentially quantified and treat it as [_] (which is ok,
   assuming there are no constraints). *)
open! Base
open! Ppxlib

type t

(** Renaming for contexts outside a type declaration, such as expression extensions. *)
val without_type : unit -> t

(** Renaming for a type declaration. Adds [prefix] to bindings for type parameters. *)
val of_type_declaration : type_declaration -> prefix:string -> t

(** Adds a new name with the given [prefix] for a universally bound type variable. *)
val add_universally_bound : t -> string loc -> prefix:string -> t

module Binding_kind : sig
  type t =
    | Universally_bound of Fresh_name.t
    | Existentially_bound
end

(** Looks up the binding for a type variable. *)
val binding_kind : t -> string -> loc:location -> Binding_kind.t

(** Extends the renaming of a type declaration with GADT context for a constructor
    declaration, if any. *)
val with_constructor_declaration
  :  t
  -> constructor_declaration
  -> type_parameters:string list
  -> t
