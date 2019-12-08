(** Universal/heterogeneous maps, useful for storing values of arbitrary type in a single
    map.

    In order to recover a value, it must be looked up with exactly the [Key.t] it was
    stored in. In other words, given different [Key.t]s from the same [string], one will
    not be able to recover the key stored in the other one.

    This is similar to [Univ] in spirit.
*)

open! Import
include module type of Univ_map_intf
include S with type 'a data = 'a

module Make (Data : sig
    type 'a t [@@deriving sexp_of]
  end) : S with type 'a data = 'a Data.t

module Make1 (Data : sig
    type ('s, 'a) t [@@deriving sexp_of]
  end) : S1 with type ('s, 'a) data = ('s, 'a) Data.t

(** keys with associated default values, so that [find] is no longer partial *)
module With_default : sig
  module Key : sig
    type 'a t

    val create : default:'a -> name:string -> ('a -> Sexp.t) -> 'a t
    val id : 'a t -> 'a Type_equal.Id.t
  end

  val set : t -> 'a Key.t -> 'a -> t
  val find : t -> 'a Key.t -> 'a
  val change : t -> 'a Key.t -> f:('a -> 'a) -> t
end

(** keys that map to an accumulator value with an associated fold operation *)
module With_fold : sig
  module Key : sig
    type ('a, 'b) t

    val create
      :  init:'b
      -> f:('b -> 'a -> 'b)
      -> name:string
      -> ('b -> Sexp.t)
      -> ('a, 'b) t

    val id : ('a, 'b) t -> 'b Type_equal.Id.t
  end

  (** reset the accumulator *)
  val set : t -> ('a, 'b) Key.t -> 'b -> t

  (** the current accumulator *)
  val find : t -> ('a, 'b) Key.t -> 'b

  (** fold value into accumulator *)
  val add : t -> ('a, 'b) Key.t -> 'a -> t

  (** accumulator update *)
  val change : t -> ('a, 'b) Key.t -> f:('b -> 'b) -> t
end

(** list-accumulating keys with a default value of the empty list *)
module Multi : sig
  module Key : sig
    type 'a t

    val create : name:string -> ('a -> Sexp.t) -> 'a t
    val id : 'a t -> 'a list Type_equal.Id.t
  end

  val set : t -> 'a Key.t -> 'a list -> t
  val find : t -> 'a Key.t -> 'a list
  val add : t -> 'a Key.t -> 'a -> t
  val change : t -> 'a Key.t -> f:('a list -> 'a list) -> t
end
