(** This module is not exposed for external use, and is only here for the implementation
    of [Uniform_array] internally.  [Obj.t Uniform_array.t] should be used in place of
    [Obj_array.t].  *)

open! Import

type t [@@deriving_inline sexp_of]

val sexp_of_t : t -> Sexplib0.Sexp.t

[@@@end]

include Blit.S with type t := t
include Invariant.S with type t := t

(** [create ~len x] returns an obj-array of length [len], all of whose indices have value
    [x]. *)
val create : len:int -> Caml.Obj.t -> t

(** [create_zero ~len] returns an obj-array of length [len], all of whose indices have
    value [Caml.Obj.repr 0]. *)
val create_zero : len:int -> t

(** [copy t] returns a new array with the same elements as [t]. *)
val copy : t -> t

val singleton : Caml.Obj.t -> t
val empty : t
val length : t -> int

(** [get t i] and [unsafe_get t i] return the object at index [i].  [set t i o] and
    [unsafe_set t i o] set index [i] to [o].  In no case is the object copied.  The
    [unsafe_*] variants omit the bounds check of [i]. *)
val get : t -> int -> Caml.Obj.t

val unsafe_get : t -> int -> Caml.Obj.t
val set : t -> int -> Caml.Obj.t -> unit
val unsafe_set : t -> int -> Caml.Obj.t -> unit
val swap : t -> int -> int -> unit

(** [set_with_caml_modify] simply sets the value in the array with no bells and whistles,
    unlike [set] which first reads the value to optimize immediate values and setting the
    index to its current value. This can be used when these optimizations are not useful,
    but the noise in generated code is annoying (and might have an impact on performance,
    although this is pure speculation). *)
val set_with_caml_modify : t -> int -> Caml.Obj.t -> unit

(** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to [obj], but only
    works correctly if [Caml.Obj.is_int (get t i)].  This precondition saves a dynamic
    check.

    [unsafe_set_int_assuming_currently_int] is similar, except the value being set is an
    int.

    [unsafe_set_int] is similar but does not assume anything about the target. *)
val unsafe_set_assuming_currently_int : t -> int -> Caml.Obj.t -> unit


val unsafe_set_int_assuming_currently_int : t -> int -> int -> unit
val unsafe_set_int : t -> int -> int -> unit

(** [unsafe_set_omit_phys_equal_check] is like [unsafe_set], except it doesn't do a
    [phys_equal] check to try to skip [caml_modify].  It is safe to call this even if the
    values are [phys_equal]. *)
val unsafe_set_omit_phys_equal_check : t -> int -> Caml.Obj.t -> unit

(** Same as [set_with_caml_modify], but without bounds checks. This is like
    [unsafe_set_omit_phys_equal_check] except it doesn't check whether the old value and
    the value being set are integers to try to skip [caml_modify]. *)
val unsafe_set_with_caml_modify : t -> int -> Caml.Obj.t -> unit

(** [unsafe_clear_if_pointer t i] prevents [t.(i)] from pointing to anything to prevent
    space leaks.  It does this by setting [t.(i)] to [Caml.Obj.repr 0].  As a performance hack,
    it only does this when [not (Caml.Obj.is_int t.(i))]. *)
val unsafe_clear_if_pointer : t -> int -> unit
