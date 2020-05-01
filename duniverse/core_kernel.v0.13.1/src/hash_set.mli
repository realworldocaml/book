(** A mutable set of elements. *)

open! Import
open Hash_set_intf

type 'a t = 'a Base.Hash_set.t [@@deriving sexp_of]

(** We use [[@@deriving sexp_of]] but not [[@@deriving sexp]] because we want people to be
    explicit about the hash and comparison functions used when creating hashtables.  One
    can use [Hash_set.Poly.t], which does have [[@@deriving sexp]], to use polymorphic
    comparison and hashing. *)

include Creators with type 'a t := 'a t
include Accessors with type 'a t := 'a t with type 'a elt := 'a elt

val hashable : 'key t -> 'key Hashtbl.Hashable.t

module type Elt_plain = Hashtbl.Key_plain
module type Elt = Hashtbl.Key
module type Elt_binable = Hashtbl.Key_binable
module type S_plain = S_plain with type 'a hash_set := 'a t
module type S = S with type 'a hash_set := 'a t
module type S_binable = S_binable with type 'a hash_set := 'a t

module Using_hashable : sig
  include
    Creators_generic
    with type 'a t := 'a t
    with type 'a elt = 'a
    with type ('key, 'z) create_options :=
      ('key, 'z) create_options_with_hashable_required
end

(** A hash set that uses polymorphic comparison. *)
module Poly : sig
  type nonrec 'a t = 'a t [@@deriving sexp]

  include
    Creators_generic
    with type 'a t := 'a t
    with type 'a elt = 'a
    with type ('key, 'z) create_options :=
      ('key, 'z) create_options_without_first_class_module

  include Accessors with type 'a t := 'a t with type 'a elt := 'a elt
end

module Make_plain (Elt : Elt_plain) : S_plain with type elt = Elt.t
module Make (Elt : Elt) : S with type elt = Elt.t
module Make_binable (Elt : Elt_binable) : S_binable with type elt = Elt.t

module M (Elt : T.T) : sig
  type nonrec t = Elt.t t
end

module type Sexp_of_m = sig
  type t [@@deriving sexp_of]
end

module type M_of_sexp = sig
  type t [@@deriving of_sexp]

  include Hashtbl_intf.Key with type t := t
end

val sexp_of_m__t : (module Sexp_of_m with type t = 'elt) -> 'elt t -> Base.Sexp.t
val m__t_of_sexp : (module M_of_sexp with type t = 'elt) -> Base.Sexp.t -> 'elt t
