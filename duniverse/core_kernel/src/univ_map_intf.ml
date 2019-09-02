open! Import
module Key = Type_equal.Id

module type S = sig
  type t [@@deriving sexp_of]
  type 'a data

  include Invariant.S with type t := t

  val empty : t
  val is_empty : t -> bool
  val set : t -> 'a Key.t -> 'a data -> t
  val mem : t -> 'a Key.t -> bool
  val mem_by_id : t -> Key.Uid.t -> bool
  val find : t -> 'a Key.t -> 'a data option
  val find_exn : t -> 'a Key.t -> 'a data
  val add : t -> 'a Key.t -> 'a data -> [`Ok of t | `Duplicate]
  val add_exn : t -> 'a Key.t -> 'a data -> t
  val change : t -> 'a Key.t -> f:('a data option -> 'a data option) -> t
  val change_exn : t -> 'a Key.t -> f:('a data -> 'a data) -> t
  val update : t -> 'a Key.t -> f:('a data option -> 'a data) -> t
  val remove : t -> 'a Key.t -> t
  val remove_by_id : t -> Key.Uid.t -> t

  module Packed : sig
    type t = T : 'a Key.t * 'a data -> t
  end

  val to_alist : t -> Packed.t list
  val of_alist_exn : Packed.t list -> t
end

module type S1 = sig
  (** The ['s] parameter is shared across all values stored in the map. *)
  type 's t [@@deriving sexp_of]

  type ('s, 'a) data

  val invariant : _ t -> unit
  val empty : _ t
  val is_empty : _ t -> bool
  val set : 's t -> 'a Key.t -> ('s, 'a) data -> 's t
  val mem : _ t -> _ Key.t -> bool
  val mem_by_id : _ t -> Key.Uid.t -> bool
  val find : 's t -> 'a Key.t -> ('s, 'a) data option
  val find_exn : 's t -> 'a Key.t -> ('s, 'a) data
  val add : 's t -> 'a Key.t -> ('s, 'a) data -> [`Ok of 's t | `Duplicate]
  val add_exn : 's t -> 'a Key.t -> ('s, 'a) data -> 's t

  val change
    :  's t
    -> 'a Key.t
    -> f:(('s, 'a) data option -> ('s, 'a) data option)
    -> 's t

  val change_exn : 's t -> 'a Key.t -> f:(('s, 'a) data -> ('s, 'a) data) -> 's t
  val update : 's t -> 'a Key.t -> f:(('s, 'a) data option -> ('s, 'a) data) -> 's t
  val remove : 's t -> 'a Key.t -> 's t
  val remove_by_id : 's t -> Key.Uid.t -> 's t

  module Packed : sig
    type 's t = T : 'a Key.t * ('s, 'a) data -> 's t
  end

  val to_alist : 's t -> 's Packed.t list
  val of_alist_exn : 's Packed.t list -> 's t
end
