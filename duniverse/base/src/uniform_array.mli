(** Same semantics as ['a Array.t], except it's guaranteed that the representation array
    is not tagged with [Double_array_tag], the tag for float arrays.

    This means it's safer to use in the presence of [Obj.magic], but it's slower than
    normal [Array] if you use it with floats.

    It can often be faster than [Array] if you use it with non-floats. *)


open! Import

(** See [Base.Array] for comments. *)
type 'a t [@@deriving_inline sexp]

include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t := 'a t

[@@@end]

val invariant : _ t -> unit


val empty : _ t
val create : len:int -> 'a -> 'a t
val singleton : 'a -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val length : 'a t -> int
val get : 'a t -> int -> 'a
val unsafe_get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val unsafe_set : 'a t -> int -> 'a -> unit
val swap : _ t -> int -> int -> unit

(** [unsafe_set_omit_phys_equal_check] is like [unsafe_set], except it doesn't do a
    [phys_equal] check to try to skip [caml_modify].  It is safe to call this even if the
    values are [phys_equal]. *)
val unsafe_set_omit_phys_equal_check : 'a t -> int -> 'a -> unit

val map : 'a t -> f:('a -> 'b) -> 'b t
val iter : 'a t -> f:('a -> unit) -> unit

(** Like {!iter}, but the function is applied to the index of the element as first
    argument, and the element itself as second argument. *)
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit

(** [of_array] and [to_array] return fresh arrays with the same contents rather than
    returning a reference to the underlying array. *)
val of_array : 'a array -> 'a t

val to_array : 'a t -> 'a array
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list

include Blit.S1 with type 'a t := 'a t

val copy : 'a t -> 'a t

(** {2 Extra lowlevel and unsafe functions} *)

(** The behavior is undefined if you access an element before setting it. *)
val unsafe_create_uninitialized : len:int -> _ t

(** New obj array filled with [Obj.repr 0] *)
val create_obj_array : len:int -> Caml.Obj.t t

(** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to [obj], but only
    works correctly if the value there is an immediate, i.e. [Caml.Obj.is_int (get t i)].
    This precondition saves a dynamic check.

    [unsafe_set_int_assuming_currently_int] is similar, except the value being set is an
    int.

    [unsafe_set_int] is similar but does not assume anything about the target. *)
val unsafe_set_assuming_currently_int : Caml.Obj.t t -> int -> Caml.Obj.t -> unit

val unsafe_set_int_assuming_currently_int : Caml.Obj.t t -> int -> int -> unit
val unsafe_set_int : Caml.Obj.t t -> int -> int -> unit


(** [unsafe_clear_if_pointer t i] prevents [t.(i)] from pointing to anything to prevent
    space leaks.  It does this by setting [t.(i)] to [Caml.Obj.repr 0].  As a performance
    hack, it only does this when [not (Caml.Obj.is_int t.(i))].  It is an error to access
    the cleared index before setting it again. *)
val unsafe_clear_if_pointer : Caml.Obj.t t -> int -> unit

(** As [Array.exists]. *)
val exists : 'a t -> f:('a -> bool) -> bool

(** Functions with the 2 suffix raise an exception if the lengths of the two given arrays
    aren't the same. *)
val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val min_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
val max_elt : 'a t -> compare:('a -> 'a -> int) -> 'a option
