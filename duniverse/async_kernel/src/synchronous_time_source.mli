(** A synchronous version of [Async_kernel.Time_source].  [advance_by_alarms] runs
    alarms immediately, rather than enqueueing Async jobs.

    [Synchronous_time_source] is a wrapper around [Timing_wheel].  One difference is
    that [Synchronous_time_source] alarms fire in non-decreasing time order, whereas in
    [Timing_wheel] that is only true for alarms in different time intervals as
    determined by [alarm_precision]. *)

open! Core
open! Import

module T1 : sig
  type -'rw t = 'rw Types.Time_source.t1 [@@deriving sexp_of]
end

module Read_write : sig
  (** With read permission you can get the current time and schedule alarms.
      With write permission you can advance time and inspect the event queue. *)
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

(** [run_at t at f] schedules an alarm that will run [f] during the next subsequent
    [advance_by_alarms t ~to_] that causes [now t >= at].  If [at <= now t], then [f] will
    to run at the next call to [advance_by_alarms].  [f] is allowed to do all
    [Synchronous_time_source] operations except for [advance_by_alarms] (because [f] is
    already running during [advance_by_alarms].  Adding alarms is not zero-alloc and the
    underlying events live in the OCaml heap. *)
val run_at : [> read ] T1.t -> Time_ns.t -> callback -> unit

(** [run_after t span f] is [run_at t (now t + span) f]. *)
val run_after : [> read ] T1.t -> Time_ns.Span.t -> callback -> unit

(** [run_at_intervals t span f] schedules [f] to run at intervals [now t + k * span], for
    k = 0, 1, 2, etc.  [run_at_intervals] raises if [span < alarm_precision t]. *)
val run_at_intervals : [> read ] T1.t -> Time_ns.Span.t -> callback -> unit

(** [max_allowed_alarm_time t] returns the greatest [at] that can be supplied to [add].
    [max_allowed_alarm_time] is not constant; its value increases as [now t] increases. *)
val max_allowed_alarm_time : [> read ] T1.t -> Time_ns.t

(** [duration_of t f] invokes [f] and measures how long it takes for the call to finish. *)
val duration_of : [> read ] T1.t -> (unit -> 'a) -> 'a * Time_ns.Span.t

module Event : sig
  type t [@@deriving sexp_of]

  include Invariant.S with type t := t

  module Option : sig
    type value := t
    type t [@@deriving sexp_of]

    (** Constructors analogous to [None] and [Some].  If [not (some_is_representable x)]
        then [some x] may raise or return [none]. *)

    val none : t
    val some : value -> t
    val is_none : t -> bool
    val is_some : t -> bool

    (** [value (some x) ~default = x] and [value none ~default = default]. *)
    val value : t -> default:value -> value

    (** [value_exn (some x) = x].  [value_exn none] raises.  Unlike [Option.value_exn],
        there is no [?message] argument, so that calls to [value_exn] that do not raise
        also do not have to allocate. *)
    val value_exn : t -> value

    val to_option : t -> value option
    val of_option : value option -> t

    module Optional_syntax : Optional_syntax.S with type t := t with type value := value
  end

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
      callback and the event is not scheduled at intervals. *)
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

  (** [reschedule_at timesource t time] updates [t] to next fire at [time].

      For periodic events, [reschedule] updates the next time to fire, and
      leaves the interval unchanged.  Events rescheduled to a past time will
      fire at the next advance of [timesource]. *)
  val reschedule_at : [> read ] T1.t -> t -> Time_ns.t -> unit

  val reschedule_after : [> read ] T1.t -> t -> Time_ns.Span.t -> unit

  (** [scheduled_at] returns the time that the event is currently scheduled at *)
  val scheduled_at : t -> Time_ns.t
end

val default_timing_wheel_config : Timing_wheel.Config.t

(** A time source with [now t] given by wall-clock time (i.e. [Time_ns.now]), and
    automatically advanced at the start of each Async cycle.  The wall clock uses the same
    timing wheel as that used by the Async scheduler, and is hence similarly affected by
    the [ASYNC_CONFIG] environment variable. *)
val wall_clock : unit -> t

(** {2 For Scheduler Implementors} *)

(** [length t] returns the number of alarms in the underlying [Timing_wheel]. *)
val length : [> write ] T1.t -> int

(** [next_alarm_runs_at t] returns a time to which the clock can be advanced
    such that an alarm will fire, or [None] if [t] has no alarms that can ever fire.

    Note that this is not necessarily the minimum such time, but it's within
    [alarm_precision] of that.

    If an alarm was already fired (e.g. because it was scheduled in the past), but
    its callbacks were not run yet, this function returns [Some now], to indicate
    that a trivial time advancement is sufficient for those to run.
*)
val next_alarm_runs_at : [> write ] T1.t -> Time_ns.t option

val next_alarm_fires_at : [> write ] T1.t -> Time_ns.t option
[@@deprecated "[since 2021-06] Use [next_alarm_runs_at]"]

(** [advance_by_alarms t ~to_] advances [t]'s time to [to_], running callbacks for all
    alarms in [t] whose [at <= to_].  Callbacks run in nondecreasing order of [at].  If
    [to_ <= now t], then [now t] does not change (and in particular does not go backward),
    but alarms with [at <= to_] may still may fire. *)
val advance_by_alarms : [> write ] T1.t -> to_:Time_ns.t -> unit Or_error.t

(** A version of [advance_by_alarms] with some weird behavior caused by timing wheel
    [alarm_precision]: if there are multiple alarms within the same timing_wheel
    precision bucket, then this function fires them all at the same time (when the last
    of the bunch of alarms is supposed to fire).
    The time  [to_] counts as an alarm for this purpose. (any alarms in the same bucket as
    [to_] will be fired at time [to_].

    [advance_by_alarms] has no such weirdness, and fires every alarm at the time that
    alarm is scheduled.
*)
val advance_by_max_alarms_in_each_timing_wheel_interval
  :  [> write ] T1.t
  -> to_:Time_ns.t
  -> unit Or_error.t

(** Instead of [advance_directly], you probably should use [advance_by_alarms].
    [advance_directly t ~to_] advances the clock directly to [to_], whereas
    [advance_by_alarms] advances the clock in steps, to each intervening alarm.  In
    particular periodic/rearming timers will fire at most twice. *)
val advance_directly : [> write ] T1.t -> to_:Time_ns.t -> unit Or_error.t


(** This value is close to [next_alarm_fires_at] but differs from it by at most
    [alarm_precision]. Requires a more expensive iteration of alarms.

    This is a closer approximation of the minimum time at which an alarm will fire,
    but it's still not there (you need min_alarm_time_... for that). *)
val max_alarm_time_in_min_timing_wheel_interval : [> write ] T1.t -> Time_ns.t option

(** Returns true iff there is work to do without advancing time further. (This can be
    caused by scheduling events in the past, or starting a recurring event.) *)
val has_events_to_run : [> write ] T1.t -> bool
