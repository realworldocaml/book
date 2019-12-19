(** A set of busy-poll functions.

    See {!Scheduler.add_busy_poller} for the user-level interface. *)

open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

(** [create ()] creates a new empty set. *)
val create : unit -> t

val is_empty : t -> bool

(** [add t f] adds function [f] to the set [t].  [f] will run every time [poll] is called.
    [f] runs with the same execution context that was in effect when [add] was called.
    When [poll] is called and [f] returns [`Stop_polling], the result of [add] becomes
    determined and [f] is removed from the set.  Also, if [f] raises, it is removed from
    the set and the exception is sent to the monitor in effect when [add] was called, *)
val add : t -> (unit -> [ `Stop_polling of 'a | `Continue_polling ]) -> 'a Deferred.t

(** [poll t] runs every function in the set [t]. *)
val poll : t -> unit
