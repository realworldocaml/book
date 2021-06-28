(** [Eager_deferred] partially implements the [Deferred] interface, with a type ['a t]
    equal to ['a Deferred.t], but where the operations are "eager", that is built upon a
    world where [bind], [map], and [upon] eagerly apply their closure without preemption
    in the case the deferred they are working with is already determined.

    The goal with that approach is that one can locally write the following to switch to
    such a world.

    {[ open Eager_deferred.Use ]}

    We do not intend at first for this to implement the entire [Deferred] interface,
    because some of this will require more experimentation and discussions.  We can
    proceed incrementally to enrich this interface.

    [test/test_eager_deferred] verifies that this interface is a sub interface of the
    [Deferred] interface.  For documentation, refer to
    {{!Async_kernel.Deferred}[Deferred]}. *)

open! Core_kernel
open! Async_kernel
open! Import

include
  Eager_deferred_intf.Eager_deferred1
  (*_ We do not expose [Eager_deferred.t] so that type-error messages refer to
    [Deferred.t], not [Eager_deferred.t]. *)
  with type 'a t := 'a Deferred.t
