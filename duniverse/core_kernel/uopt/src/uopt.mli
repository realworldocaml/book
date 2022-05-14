(** A [Uopt.t] is an unboxed option.  This module is tricky and potentially unsafe.  It
    should mostly not be used, and when it is, one must keep the [Uopt.t] inside their
    module, and not expose them (not even if they are given an abstract type).

    ['a Uopt.t] is like ['a option], but doesn't box [some] values.  It must not be used
    in a nested way, i.e. as ['a Uopt.t Uopt.t].  It must also not be used for [float
    Uopt.t array], since the representation of the array would vary depending on whether
    [none] or [some] is used to create the array, but [float Uopt.t Uniform_array.t] is
    fine.  It should also not be used in a record that contains only monomorphic [float]s
    and [float Uopt.t]s, because the compiler would treat that as a float-only record and
    would unbox the record fields (as described in the documentation for writing C
    bindings).

    Since ['a Uopt.t] is abtract, manipulation of an ['a Uopt.t array] does runtime checks
    to see if this is a float array. This can be mostly avoided with [Uniform_array.t],
    although array creation will still do such checks, and you may want to use the
    [set_with_caml_modify] kind of function to skip the immediacy checks. *)


open! Core

type +'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val none : _ t
val some : 'a -> 'a t
val is_none : _ t -> bool
val is_some : _ t -> bool
val value_exn : 'a t -> 'a

(** It is safe to call [unsafe_value t] iff [is_some t]. *)
val unsafe_value : 'a t -> 'a

val to_option : 'a t -> 'a option
val of_option : 'a option -> 'a t

module Optional_syntax : Optional_syntax.S1 with type 'a t := 'a t and type 'a value := 'a
