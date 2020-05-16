(** Implements an async aware throttling rate limiter on top of [Limiter].

    All forms of [enqueue_exn] and [enqueue'] below will raise if the requested job
    is not possible to run within the resource limitations given to the related
    [create_exn].

    If any enqueued job raises then the exception will be raised to the monitor in scope
    when [enqueue_exn] is called.  Deferred jobs passed to [enqueue'] return [Raised]
    (in a deferred manner) instead.

    Jobs are always executed in FIFO order. *)

open! Core_kernel
open! Async_kernel

(** The outcome of a job *)
module Outcome : sig
  type 'a t =
    | Ok of 'a
    | Aborted
    | Raised of exn
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]
type limiter = t [@@deriving sexp_of]


(** {5 Specialized limiters}

    A collection of limiters, specialized to different use-cases, all supporting a shared
    subset of their interface *)

module type Common = sig
  type _ t

  (** kills [t], which aborts all enqueued jobs that haven't started and all jobs enqueued
      in the future.  If [t] has already been killed, then calling [kill t] has no effect.
      Note that kill does not affect currently running jobs in any way. *)
  val kill : _ t -> unit

  (** [is_dead t] returns [true] if [t] was killed, either by [kill] or by an unhandled
      exception in a job. *)
  val is_dead : _ t -> bool

  (** Convert to a limiter  *)
  val to_limiter : _ t -> limiter
end

module Token_bucket : sig
  type t [@@deriving sexp_of]
  type _ u = t
  (*_ This type synonym is introduced because older versions of OCaml
    do not support destructive substitutions with `type 'a t1 = t2`. *)

  val create_exn
    :  burst_size:int
    -> sustained_rate_per_sec:float
    -> continue_on_error:bool (** If false, then the token bucket is [kill]ed if there's
                                  an unhandled exception in any job *)
    -> ?in_flight_limit:int (** default to infinite. This can be used for concurrency
                                control *)
    -> ?initial_burst_size:int (** Defaults to zero *)
    -> unit
    -> t

  (** [enqueue_exn t x f a] enqueues an immediate job consuming [x] tokens, running [f] on
      input [a].

      if [allow_immediate_run] is true then [f] is allowed to run within the same async
      job as [enqueue_exn] iff there are enough tokens available to fully run the job and
      there are no other previously enqueued jobs that have not run.  If this is the case,
      it is run before [enqueue_exn] returns.  Otherwise no part of [f] is run before
      [enqueue_exn] returns.

      If there is a failure associated with this job then the exception will be raised to
      the monitor in scope when [enqueue_exn] is called.  Note that it may fail for a
      number of reasons, including [f] throws an exception, the limiter is killed, or the
      number of tokens requested is larger than the burst size. *)
  val enqueue_exn : t -> ?allow_immediate_run:bool -> int -> ('a -> unit) -> 'a -> unit

  (** [enqueue' t x f a] enqueues a deferred job consuming [x] tokens, running [f] on
      input [a].  No part of f is run before [enqueue'] returns. *)
  val enqueue'    : t -> int -> ('a -> 'b Deferred.t) -> 'a -> 'b Outcome.t Deferred.t

  include Common with type 'a t := 'a u
end

(** [Throttle], [Sequencer], and [Resource_throttle] re-implement the functionality
    available in the core Async.Throttle module with the hope that these implementations
    can eventually supplant that code.  It is helpful to use these modules in systems that
    can afford to do a bit more testing so that we can get feedback on the behavior of the
    new implementation.  They are intended to be mostly drop-in replacements. *)

(** Implements a basic throttle meant to bound the number of jobs that can concurrently
    run.  Additionally the [~burst_size] and [~sustained_rate_per_sec] arguments can be
    used to control how many jobs can be spawned in a burst, and how quickly jobs can be
    spawned over time.  If these options are not given to [create_exn] they are unbounded.

    [concurrent_jobs_target] is the desired maximum number of concurrent jobs.  If the
    value is never changed, then this is in fact a hard upper bound.  The value is
    mutable, however, and so may be violated temporarily if the value is reduced. *)
module Throttle : sig
  type t [@@ deriving sexp_of]
  type _ u = t
  (*_ This type synonym is introduced because older versions of OCaml
    do not support destructive substitutions with `type 'a t1 = t2`. *)

  val create_exn
    :  concurrent_jobs_target:int
    -> continue_on_error:bool
    -> ?burst_size:int
    -> ?sustained_rate_per_sec:float
    -> unit
    -> t

  val concurrent_jobs_target    : t -> int
  val num_jobs_waiting_to_start : t -> int
  val num_jobs_running          : t -> int

  val enqueue_exn        : t -> ?allow_immediate_run:bool -> ('a -> unit) -> 'a -> unit
  val enqueue'           : t -> ('a -> 'b Deferred.t) -> 'a -> 'b Outcome.t Deferred.t

  include Common with type 'a t := 'a u
end

(** A sequencer is a throttle that is specialized to only allow one job at a time and to,
    by default, not continue on error. *)
module Sequencer : sig
  type t [@@deriving sexp_of]
  type _ u = t
  (*_ This type synonym is introduced because older versions of OCaml
    do not support destructive substitutions with `type 'a t1 = t2`. *)

  val create
    :  ?continue_on_error:bool (** default is [false] *)
    -> ?burst_size:int
    -> ?sustained_rate_per_sec:float
    -> unit
    -> t

  val enqueue_exn        : t -> ?allow_immediate_run:bool -> ('a -> unit) -> 'a -> unit
  val enqueue'           : t -> ('a -> 'b Deferred.t) -> 'a -> 'b Outcome.t Deferred.t

  val num_jobs_waiting_to_start : t -> int

  include Common with type 'a t := 'a u
end

(** A resource throttle holds a static list of [n] resources that are handed out in a
    round-robin fashion to up to [n] concurrent jobs.  A resource given to [create]
    may be re-used many times in the lifetime of [t] but will never be used by more
    than one job at a time. *)
module Resource_throttle : sig
  type 'a t [@@deriving sexp_of]

  val create_exn
    :  resources:'a list
    -> continue_on_error:bool
    -> ?burst_size:int
    -> ?sustained_rate_per_sec:float
    -> unit
    -> 'a t

  val max_concurrent_jobs : _ t -> int

  val enqueue_exn        : 'a t -> ?allow_immediate_run:bool -> ('a -> unit) -> unit
  val enqueue'           : 'a t -> ('a -> 'b Deferred.t) -> 'b Outcome.t Deferred.t

  include Common with type 'a t := 'a t
end

module Expert : sig
  (** kills [t], which aborts all enqueued jobs that haven't started and all jobs enqueued
      in the future.  If [t] has already been killed, then calling [kill t] has no effect.
      Note that kill does not affect currently running jobs in any way. *)
  val kill : t -> unit

  (** [is_dead t] returns [true] if [t] was killed, either by [kill] or by an unhandled
      exception in a job. *)
  val is_dead : t -> bool

  (** returns the total cost of all jobs that have been enqueued but have not yet
      started. *)
  val cost_of_jobs_waiting_to_start : t -> int

  (** returns the underlying limiter.  It is an error to do anything with the limiter that
      isn't a read-only operation. *)
  val to_jane_limiter  : t -> Limiter.t
end
