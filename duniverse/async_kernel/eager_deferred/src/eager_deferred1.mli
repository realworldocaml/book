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
sig
  type +'a t

  include Invariant.S1 with type 'a t := 'a t
  include Monad with type 'a t := 'a t

  module Infix : sig
    include Monad.Infix with type 'a t := 'a t

    val ( >>> ) : 'a t -> ('a -> unit) -> unit
  end

  val any : 'a t list -> 'a t
  val any_unit : unit t list -> unit t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val create : ('a Ivar.t -> unit) -> 'a t
  val don't_wait_for : unit t -> unit
  val ignore : _ t -> unit t
  val is_determined : 'a t -> bool
  val never : unit -> _ t
  val ok : 'a t -> ('a, _) Core_kernel.Result.t t
  val peek : 'a t -> 'a option
  val unit : unit t
  val upon : 'a t -> ('a -> unit) -> unit
  val value_exn : 'a t -> 'a

  val repeat_until_finished
    :  'state
    -> ('state -> [`Repeat of 'state | `Finished of 'result] t)
    -> 'result t

  module List : Monad_sequence.S with type 'a monad := 'a t with type 'a t := 'a list
  module Or_error : module type of Eager_deferred_or_error
end
(*_ We do not expose [Eager_deferred.t] so that type-error messages refer to
  [Deferred.t], not [Eager_deferred.t]. *)
with type 'a t := 'a Deferred.t
