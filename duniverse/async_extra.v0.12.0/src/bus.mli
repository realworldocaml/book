(** An Async extension of {{!Core_kernel.Bus}[Core_kernel.Bus]}.  Functions that share the
    same name and types as those in [Core_kernel.Bus] are direct calls to same. *)

open! Core
open! Import

(** @open *)
include module type of struct
  include Core_kernel.Bus
end

(** [pipe1_exn t] returns a pipe of updates from [t] by subscribing to [t].  Closing the
    pipe unsubscribes from [t].  Closing [t] closes the pipe.  [pipe1_exn] raises in the
    same circumstances as [subscribe_exn]. *)
val pipe1_exn : ('a -> unit) Read_only.t -> Source_code_position.t -> 'a Pipe.Reader.t

module First_arity : sig
  type (_, _, _) t =
    | Arity1 : ('a -> unit, 'a -> 'r option, 'r) t
    | Arity2 : ('a -> 'b -> unit, 'a -> 'b -> 'r option, 'r) t
    | Arity3 : ('a -> 'b -> 'c -> unit, 'a -> 'b -> 'c -> 'r option, 'r) t
    | Arity4 : ('a -> 'b -> 'c -> 'd -> unit, 'a -> 'b -> 'c -> 'd -> 'r option, 'r) t
  [@@deriving sexp_of]
end

(** [first_exn here t arity ~f] returns a deferred that becomes determined with value [r]
    when the first event is published to [t] where [f] returns [Some r].  [first_exn] then
    unsubscribes from [t], ensuring that [f] is never called again after it returns
    [Some].  [first_exn] raises if it can't subscribe to the bus, i.e., if [subscribe_exn]
    raises.  If [f] raises, then [first_exn] raises to the monitor in effect when
    [first_exn] was called.  [first_exn] takes time proportional to the number of bus
    subscribers.

    If [stop] is provided and becomes determined, [f] will not be called again, it will
    unsubscribe from the bus, and the deferred that was returned by [first_exn] will never
    become determined. *)
val first_exn
  :  ?stop:unit Deferred.t
  -> 'c Read_only.t
  -> Source_code_position.t
  -> ('c, 'f, 'r) First_arity.t
  -> f:'f
  -> 'r Deferred.t
