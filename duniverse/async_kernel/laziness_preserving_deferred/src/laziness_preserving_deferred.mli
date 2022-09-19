open Core
open Async_kernel

(** [Laziness_preserving_deferred] offers a monad for working with lazy deferreds that has
    similar semantics to the [Deferred] monad. In the regular [Lazy_deferred] monad, each
    [map/bind] introduces a new lazy computation, and nothing runs unless explicitly
    forced. By contrast, this monad lets you build up a tree of computations that can be
    "weakly run," meaning that it will run until it encounters an unforced lazy deferred,
    at which point it will wait for that lazy deferred to be forced before continuing. *)

type 'a t

include Monad.S with type 'a t := 'a t

val of_eager : 'a Deferred.t -> 'a t
val of_lazy : 'a Lazy_deferred.t -> 'a t

(** Begin computing ['a t], forcing lazy deferreds as they are encountered. *)
val force : 'a t -> 'a Deferred.Or_error.t

(** Begin computing ['a t], waiting on lazy deferreds as they are encountered. *)
val weak_run : 'a t -> 'a Deferred.Or_error.t
