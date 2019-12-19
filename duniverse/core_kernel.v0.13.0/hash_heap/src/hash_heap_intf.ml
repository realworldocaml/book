open! Core_kernel
open! Import

module type Key = Hashtbl.Key_plain

module type S = sig
  module Key : Key

  type 'a t

  val create : ?min_size:int -> ('a -> 'a -> int) -> 'a t
  val copy : 'a t -> 'a t
  val push : 'a t -> key:Key.t -> data:'a -> [ `Ok | `Key_already_present ]
  val push_exn : 'a t -> key:Key.t -> data:'a -> unit
  val replace : 'a t -> key:Key.t -> data:'a -> unit
  val remove : 'a t -> Key.t -> unit
  val mem : 'a t -> Key.t -> bool
  val top : 'a t -> 'a option
  val top_exn : 'a t -> 'a
  val top_with_key : 'a t -> (Key.t * 'a) option
  val top_with_key_exn : 'a t -> Key.t * 'a
  val pop_with_key : 'a t -> (Key.t * 'a) option
  val pop_with_key_exn : 'a t -> Key.t * 'a
  val pop : 'a t -> 'a option
  val pop_exn : 'a t -> 'a
  val pop_if_with_key : 'a t -> (key:Key.t -> data:'a -> bool) -> (Key.t * 'a) option
  val pop_if : 'a t -> ('a -> bool) -> 'a option
  val find : 'a t -> Key.t -> 'a option
  val find_pop : 'a t -> Key.t -> 'a option
  val find_exn : 'a t -> Key.t -> 'a
  val find_pop_exn : 'a t -> Key.t -> 'a

  (** Mutation of the heap during iteration is not supported, but there is no check to
      prevent it.  The behavior of a heap that is mutated during iteration is
      undefined. *)
  val iter_keys : _ t -> f:(Key.t -> unit) -> unit

  val iter : 'a t -> f:('a -> unit) -> unit
  val iteri : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit

  (** Returns the list of all (key, value) pairs for given [Hash_heap]. *)
  val to_alist : 'a t -> (Key.t * 'a) list

  val length : 'a t -> int
end
