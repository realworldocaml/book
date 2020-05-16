(** A synchronous version of [Async_kernel.Time_source].  [advance_by_alarms] runs
    alarms immediately, rather than enqueueing Async jobs.

    [Synchronous_time_source] is a wrapper around [Timing_wheel].  One difference is
    that [Synchronous_time_source] alarms fire in non-decreasing time order, whereas in
    [Timing_wheel] that is only true for alarms in different time intervals as
    determined by [alarm_precision]. *)

open! Core_kernel
open! Import

module T1 : sig
  type -'rw t = 'rw Types.Time_source.t1 [@@deriving sexp_of]
end

module Read_write : sig
  type t = read_write T1.t [@@deriving sexp_of]

  include Invariant.S with type t := t

  val invariant_with_jobs : job:Job.t Invariant.t -> t Invariant.t
end

module Id : Unique_id.Id

type t = read T1.t [@@deriving sexp_of]

val invariant_with_jobs : job:Job.t Invariant.t -> t Invariant.t

include Invariant.S with type t := t

(** [id t] returns a unique, consistent identifier which can be used e.g. as a map or hash
    table key. *)
val id : _ T1.t -> Id.t

(** [length t] returns the number of alarms in the underlying [Timing_wheel]. *)
val length : _ T1.t -> int

val read_only : [> read ] T1.t -> t

type callback = unit -> unit

(** [create ~now ()] creates a new time source.  The default [timing_wheel_config] has 100
    microsecond precision, with levels of >1s, >1m, >1h, >1d.  The [timing_wheel_config]
    is used to tune performance; configuration does not affect the fact that alarms fire
    in non-decreasing time order. *)
val create
  :  ?timing_wheel_config:Timing_wheel.Config.t
  -> now:Time_ns.t
  -> unit
  -> read_write T1.t

val alarm_precision : [> read ] T1.t -> Time_ns.Span.t

(** [is_wall_clock] reports whether this time source represents 'wall clock' time, or some
    alternate source of time. *)
val is_wall_clock : [> read ] T1.t -> bool

(** The behavior of [now] is special for [wall_clock ()]; it always calls [Time_ns.now
    ()], so it can return times that the time source has not yet been advanced to. *)
val now : [> read ] T1.t -> Time_ns.t

(** Removes the special behavior of [now] for [wall_clock ()]; it always returns the
    timing wheel's notion of now, which means that the following inequality always holds:
    [timing_wheel_now () <= now ()]. *)
val timing_wheel_now : [> read ] T1.t -> Time_ns.t

(** [advance_by_alarms t ~to_] advances [t]'s time to [to_], running callbacks for all
    alarms in [t] whose [at <= to_].  Callbacks run in nondecreasing order of [at].  If
    [to_ <= now t], then [now t] does not change (and in particular does not go backward),
    but alarms with [at <= to_] may still may fire. *)
val advance_by_alarms : [> write ] T1.t -> to_:Time_ns.t -> unit Or_error.t

(** Instead of [advance_directly], you probably should use [advance_by_alarms].
    [advance_directly t ~to_] advances the clock directly to [to_], whereas
    [advance_by_alarms] advances the clock in steps, to each intervening alarm.  In
    particular periodic/rearming timers will fire at most twice. *)
val advance_directly : [> write ] T1.t -> to_:Time_ns.t -> unit Or_error.t

(** [run_at t at f] schedules an alarm that will run [f] during the next subsequent
    [advance_by_alarms t ~to_] that causes [now t >= at].  If [at <= now t], then [f] will
    to run at the next call to [advance_by_alarms].  [f] is allowed to do all
    [Synchronous_time_source] operations except for [advance_by_alarms] (because [f] is
    already running during [advance_by_alarms].  Adding alarms is not zero-alloc and the
    underlying events live in the OCaml heap. *)
val run_at : [> read ] T1.t -> Time_ns.t -> callback -> unit

(** [run_after t span f] is [run_at t (now t + span) f]. *)
val run_after : [> read ] T1.t -> Time_ns.Span.t -> callback -> unit

(** [run_at_intervals t span f] causes [f] to run at intervals [now t + k * span], for
    k = 0, 1, 2, etc.  [run_at_intervals] raises if [span < alarm_precision t]. *)
val run_at_intervals : [> read ] T1.t -> Time_ns.Span.t -> callback -> unit

(** [max_allowed_alarm_time t] returns the greatest [at] that can be supplied to [add].
    [max_allowed_alarm_time] is not constant; its value increases as [now t] increases. *)
val max_allowed_alarm_time : [> read ] T1.t -> Time_ns.t

module Event : sig
  type t [@@deriving sexp_of]

  include Invariant.S with type t := t

  (** These are like the corresponding [run_*] functions, except they return an event that
      one can later [abort]. *)
  val at : [> read ] T1.t -> Time_ns.t -> callback -> t

  val after : [> read ] T1.t -> Time_ns.Span.t -> callback -> t
  val at_intervals : [> read ] T1.t -> Time_ns.Span.t -> callback -> t

  module Abort_result : sig
    type t =
      | Ok
      | Currently_happening
      | Previously_unscheduled
    [@@deriving sexp_of]
  end

  (** [abort t] aborts the event [t], if possible, and returns [Ok] if the event was
      aborted, or the reason it could not be aborted.  [abort] returns
      [Currently_happening] iff it is called on an event while running that event's
      callback. *)
  val abort : [> read ] T1.t -> t -> Abort_result.t

  val abort_exn : [> read ] T1.t -> t -> unit
  val abort_if_possible : [> read ] T1.t -> t -> unit

  (** [create timesource callback] creates an event that is not scheduled in
      [timesource]'s timing wheel but is available to be scheduled using [schedule_at] and
      [schedule_after]. *)
  val create : [> read ] T1.t -> callback -> t

  (** [schedule_at timesource t time] schedules [t] to fire at [time].  [schedule_at]
      returns [Error] if [t] is currently scheduled to run. *)
  val schedule_at : [> read ] T1.t -> t -> Time_ns.t -> unit Or_error.t

  val schedule_after : [> read ] T1.t -> t -> Time_ns.Span.t -> unit Or_error.t
  val schedule_at_intervals : [> read ] T1.t -> t -> Time_ns.Span.t -> unit Or_error.t

  module Reschedule_result : sig
    (** [Recently_aborted] and [Recently_fired] can happen if the time was not advanced
        between the corresponding thing happening and the call to [reschedule*]. *)
    type t =
      | Ok
      | Currently_happening
      | Recently_aborted
      | Recently_fired
    [@@deriving sexp_of]
  end

  (** [reschedule_at timesource t time] attempts to update [t] to next fire at [time].
      This returns [Ok] for events that are unscheduled, or scheduled but not yet fired.
      If the event is in the process of firing or being aborted, [reschedule_at] does not
      succeed.  For periodic events, [reschedule] updates the next time to fire, and leave
      the interval unchanged.  Events rescheduled to a past time will fire at the next
      advance of [timesource]. *)
  val reschedule_at : [> read ] T1.t -> t -> Time_ns.t -> Reschedule_result.t

  val reschedule_after : [> read ] T1.t -> t -> Time_ns.Span.t -> Reschedule_result.t
end

val default_timing_wheel_config : Timing_wheel.Config.t

(** A time source with [now t] given by wall-clock time (i.e. [Time_ns.now]), and
    automatically advanced at the start of each Async cycle.  The wall clock uses the same
    timing wheel as that used by the Async scheduler, and is hence similarly affected by
    the [ASYNC_CONFIG] environment variable. *)
val wall_clock : unit -> t
