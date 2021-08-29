(** A time source holds a time (possibly wall-clock time, possibly simulated time) and
    gives the ability to schedule Async jobs to run when that time advances.

    There is a single wall-clock time source (returned by [wall_clock ()]) that the Async
    scheduler drives and uses for the [Clock_ns] module.  One can also create a
    user-controlled time source via [create], and advance its clock as desired.  This is
    useful so that state machines can depend on a notion of time that is distinct from
    wall-clock time. *)

open! Core_kernel
open! Import
module Deferred = Deferred1

module type Time_source = sig
  (** A time source has a phantom read-write parameter, where [write] gives permission to
      call [advance] and [fire_past_alarms]. *)
  module T1 : sig
    type -'rw t [@@deriving sexp_of]
  end

  module Read_write : sig
    type t = read_write T1.t [@@deriving sexp_of]

    include Invariant.S with type t := t

    val invariant_with_jobs : job:Job.t Invariant.t -> t Invariant.t
  end

  module Id : Unique_id.Id

  type t = read T1.t [@@deriving sexp_of]

  (** [id t] returns a unique, consistent identifier which can be used e.g. as a map or
      hash table key. *)
  val id : _ T1.t -> Id.t

  include Invariant.S with type t := t

  val invariant_with_jobs : job:Job.t Invariant.t -> t Invariant.t
  val read_only : [> read ] T1.t -> t

  val create
    :  ?timing_wheel_config:Timing_wheel.Config.t
    -> now:Time_ns.t
    -> unit
    -> read_write T1.t

  (** A time source with [now t] given by wall-clock time (i.e., [Time_ns.now]) and that
      is advanced automatically as time passes (specifically, at the start of each Async
      cycle).  There is only one wall-clock time source; every call to [wall_clock ()]
      returns the same value.  The behavior of [now] is special for [wall_clock ()]; it
      always calls [Time_ns.now ()], so it can return times that the time source has not
      yet been advanced to. *)
  val wall_clock : unit -> t

  (** Accessors.  [now (wall_clock ())] behaves specially; see [wall_clock] above. *)

  val alarm_precision : [> read ] T1.t -> Time_ns.Span.t
  val next_alarm_fires_at : [> read ] T1.t -> Time_ns.t option
  val now : [> read ] T1.t -> Time_ns.t

  (** Removes the special behavior of [now] for [wall_clock]; it always returns the
      timing_wheel's notion of now. *)
  val timing_wheel_now : [> read ] T1.t -> Time_ns.t


  (** Instead of [advance_directly], you probably should use [advance_by_alarms].
      [advance_directly t ~to_] advances the clock directly to [to_], whereas
      [advance_by_alarms] advances the clock in steps, to each intervening alarm.
      [advance_directly] approximately determines the set of events to fire, up to
      timing-wheel alarm precision, whereas [advance_by_alarms] fires all alarms whose
      time is [<= to_].  With [advance_directly], you must call [fire_past_alarms] if you
      want that behavior (see docs for [Timing_wheel.advance_clock] vs.
      [Timing_wheel.fire_past_alarms]). *)
  val advance_directly : [> write ] T1.t -> to_:Time_ns.t -> unit

  val advance : [> write ] T1.t -> to_:Time_ns.t -> unit
  [@@deprecated
    "[since 2019-06] Use [advance_directly] (to preserve behavior) or \
     [advance_by_alarms]"]

  val advance_directly_by : [> write ] T1.t -> Time_ns.Span.t -> unit

  val advance_by : [> write ] T1.t -> Time_ns.Span.t -> unit
  [@@deprecated
    "[since 2019-06] Use [advance_directly_by] (to preserve behavior) or \
     [advance_by_alarms_by]"]

  val fire_past_alarms : [> write ] T1.t -> unit

  (** [advance_by_alarms t] repeatedly calls [advance t] to drive the time forward in
      steps, where each step is the minimum of [to_] and the next alarm time. After each
      step, [advance_by_alarms] waits for the result of [wait_for] to become determined
      before advancing. By default, [wait_for] will be [Scheduler.yield ()] to allow the
      triggered timers to execute and potentially rearm for subsequent steps. The returned
      deferred is filled when [to_] is reached.

      [advance_by_alarms] is useful in simulation when one wants to efficiently advance to
      a time in the future while giving periodic timers (e.g., resulting from [every]) a
      chance to fire with approximately the same timing as they would live. *)
  val advance_by_alarms
    :  ?wait_for:(unit -> unit Deferred.t)
    -> [> write ] T1.t
    -> to_:Time_ns.t
    -> unit Deferred.t

  (** [advance_by_alarms_by ?wait_for t by] is equivalent to:
      [advance_by_alarms ?wait_for t ~to_:(Time_ns.add (now t) by)] *)
  val advance_by_alarms_by
    :  ?wait_for:(unit -> unit Deferred.t)
    -> [> write ] T1.t
    -> Time_ns.Span.t
    -> unit Deferred.t

  module Continue : sig
    type t

    val immediately : t
  end

  (** See {{!Async_kernel.Clock_intf.Clock.every'}[Clock.every]} for documentation. *)
  val run_repeatedly
    :  ?start:unit Deferred.t (** default is [return ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> ?finished:unit Ivar.t
    -> [> read ] T1.t
    -> f:(unit -> unit Deferred.t)
    -> continue:Continue.t
    -> unit

  (** The functions below here are the same as in clock_intf.ml, except they take an
      explicit [t] argument.  See {{!Async_kernel.Clock_intf}[Clock_intf]} for
      documentation. *)

  val run_at : [> read ] T1.t -> Time_ns.t -> ('a -> unit) -> 'a -> unit
  val run_after : [> read ] T1.t -> Time_ns.Span.t -> ('a -> unit) -> 'a -> unit


  val at : [> read ] T1.t -> Time_ns.t -> unit Deferred.t
  val after : [> read ] T1.t -> Time_ns.Span.t -> unit Deferred.t

  val with_timeout
    :  [> read ] T1.t
    -> Time_ns.Span.t
    -> 'a Deferred.t
    -> [ `Timeout | `Result of 'a ] Deferred.t

  module Event : sig
    type ('a, 'h) t [@@deriving sexp_of]
    type t_unit = (unit, unit) t [@@deriving sexp_of]

    include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

    val scheduled_at : (_, _) t -> Time_ns.t

    module Status : sig
      type ('a, 'h) t =
        | Aborted of 'a
        | Happened of 'h
        | Scheduled_at of Time_ns.t
      [@@deriving sexp_of]
    end

    val status : ('a, 'h) t -> ('a, 'h) Status.t
    val run_at : [> read ] T1.t -> Time_ns.t -> ('z -> 'h) -> 'z -> (_, 'h) t
    val run_after : [> read ] T1.t -> Time_ns.Span.t -> ('z -> 'h) -> 'z -> (_, 'h) t

    module Abort_result : sig
      type ('a, 'h) t =
        | Ok
        | Previously_aborted of 'a
        | Previously_happened of 'h
      [@@deriving sexp_of]
    end

    val abort : ('a, 'h) t -> 'a -> ('a, 'h) Abort_result.t
    val abort_exn : ('a, 'h) t -> 'a -> unit
    val abort_if_possible : ('a, _) t -> 'a -> unit

    module Fired : sig
      type ('a, 'h) t =
        | Aborted of 'a
        | Happened of 'h
      [@@deriving sexp_of]
    end

    val fired : ('a, 'h) t -> ('a, 'h) Fired.t Deferred.t

    module Reschedule_result : sig
      type ('a, 'h) t =
        | Ok
        | Previously_aborted of 'a
        | Previously_happened of 'h
      [@@deriving sexp_of]
    end

    val reschedule_at : ('a, 'h) t -> Time_ns.t -> ('a, 'h) Reschedule_result.t
    val reschedule_after : ('a, 'h) t -> Time_ns.Span.t -> ('a, 'h) Reschedule_result.t
    val at : [> read ] T1.t -> Time_ns.t -> (_, unit) t
    val after : [> read ] T1.t -> Time_ns.Span.t -> (_, unit) t
  end

  val at_varying_intervals
    :  ?stop:unit Deferred.t
    -> [> read ] T1.t
    -> (unit -> Time_ns.Span.t)
    -> unit Async_stream.t

  val at_intervals
    :  ?start:Time_ns.t
    -> ?stop:unit Deferred.t
    -> [> read ] T1.t
    -> Time_ns.Span.t
    -> unit Async_stream.t

  (** See {!Clock.every'} for documentation. *)
  val every'
    :  ?start:unit Deferred.t (** default is [return ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> ?finished:unit Ivar.t
    -> [> read ] T1.t
    -> Time_ns.Span.t
    -> (unit -> unit Deferred.t)
    -> unit

  val every
    :  ?start:unit Deferred.t (** default is [return ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> [> read ] T1.t
    -> Time_ns.Span.t
    -> (unit -> unit)
    -> unit

  val run_at_intervals'
    :  ?start:Time_ns.t (** default is [Time_ns.now ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> [> read ] T1.t
    -> Time_ns.Span.t
    -> (unit -> unit Deferred.t)
    -> unit

  val run_at_intervals
    :  ?start:Time_ns.t (** default is [Time_ns.now ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> [> read ] T1.t
    -> Time_ns.Span.t
    -> (unit -> unit)
    -> unit

  (** [Time_source] and [Synchronous_time_source] are the same data structure and use the
      same underlying timing wheel.  The types are freely interchangeable. *)
  val of_synchronous : 'a Synchronous_time_source.T1.t -> 'a T1.t

  val to_synchronous : 'a T1.t -> 'a Synchronous_time_source.T1.t
end
