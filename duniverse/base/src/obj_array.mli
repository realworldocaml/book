(** This module is deprecated for external use.  Users should replace occurrences of
    [Obj_array.t] in their code with [Obj.t Uniform_array.t].

    This module is here for the implementing [Uniform_array] internally, and exposed
    through [Not_exposed_properly] to ease the transition for users.
*)

open! Import

type t [@@deriving_inline sexp_of]
include
sig [@@@ocaml.warning "-32"] val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end[@@ocaml.doc "@inline"]
[@@@end]

include Blit.     S with type t := t
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
val get        : t -> int -> Caml.Obj.t
val unsafe_get : t -> int -> Caml.Obj.t
val set        : t -> int -> Caml.Obj.t -> unit
val unsafe_set : t -> int -> Caml.Obj.t -> unit

val swap : t -> int -> int -> unit

(** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to [obj], but only
    works correctly if [Caml.Obj.is_int (get t i)].  This precondition saves a dynamic
    check.

    [unsafe_set_int_assuming_currently_int] is similar, except the value being set is an
    int.

    [unsafe_set_int] is similar but does not assume anything about the target. *)
val unsafe_set_assuming_currently_int     : t -> int -> Caml.Obj.t -> unit

val unsafe_set_int_assuming_currently_int : t -> int -> int   -> unit
val unsafe_set_int                        : t -> int -> int   -> unit

(** [unsafe_set_omit_phys_equal_check] is like [unsafe_set], except it doesn't do a
    [phys_equal] check to try to skip [caml_modify].  It is safe to call this even if the
    values are [phys_equal]. *)
val unsafe_set_omit_phys_equal_check : t -> int -> Caml.Obj.t -> unit

(** [unsafe_clear_if_pointer t i] prevents [t.(i)] from pointing to anything to prevent
    space leaks.  It does this by setting [t.(i)] to [Caml.Obj.repr 0].  As a performance hack,
    it only does this when [not (Caml.Obj.is_int t.(i))]. *)
val unsafe_clear_if_pointer : t -> int -> unit

(** [truncate t ~len] shortens [t]'s length to [len].  It is an error if [len <= 0] or
    [len > length t].*)
val truncate : t -> len:int -> unit

