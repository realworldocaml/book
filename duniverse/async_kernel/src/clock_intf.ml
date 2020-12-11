(** Schedule jobs to run at a time in the future.

    The underlying implementation uses a heap of events, one for each job that needs to
    run in the future.  The Async scheduler is responsible for waking up at the right time
    to run the jobs. *)

open Core_kernel
module Deferred = Deferred1

module type Clock = sig
  module Time : sig
    module Span : sig
      type t
    end

    type t
  end

  (** [run_at time f a] runs [f a] as soon as possible after [time].  If [time] is in the
      past, then [run_at] will immediately schedule a job [t] that will run [f a].  In no
      situation will [run_at] actually call [f] itself.  The call to [f] will always be in
      another job. *)
  val run_at : Time.t -> ('a -> unit) -> 'a -> unit

  (** [run_after] is like [run_at], except that one specifies a time span rather than an
      absolute time. *)
  val run_after : Time.Span.t -> ('a -> unit) -> 'a -> unit

  (** [at time] returns a deferred [d] that will become determined as soon as possible
      after [time] *)
  val at : Time.t -> unit Deferred.t

  (** [after] is like [at], except that one specifies a time span rather than an absolute
      time.  If you set up a lot of [after] events at the beginning of your program they
      will trigger at the same time.  Use [Time.Span.randomize] to even them out. *)
  val after : Time.Span.t -> unit Deferred.t

  (** [with_timeout span d] returns a deferred that will become determined after either
      [span] elapses or [d] is determined, returning either [`Timeout] or [`Result]
      depending on which one succeeded first.  At the time the returned deferred becomes
      determined, both things may have happened, in which case [`Result] is given
      preference. *)
  val with_timeout
    :  Time.Span.t
    -> 'a Deferred.t
    -> [ `Timeout | `Result of 'a ] Deferred.t

  (** Events provide variants of [run_at] and [run_after] with the ability to abort or
      reschedule an event that hasn't yet happened.  Once an event happens or is aborted,
      Async doesn't use any space for tracking it. *)
  module Event : sig
    type ('a, 'h) t [@@deriving sexp_of]
    type t_unit = (unit, unit) t [@@deriving sexp_of]

    include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

    val scheduled_at : (_, _) t -> Time.t

    module Status : sig
      type ('a, 'h) t =
        | Aborted of 'a
        | Happened of 'h
        | Scheduled_at of Time.t
      [@@deriving sexp_of]
    end

    (** If [status] returns [Scheduled_at time], it is possible that [time < Time.now ()]
        if Async's scheduler hasn't yet gotten the chance to update its clock, e.g., due
        to user jobs running. *)
    val status : ('a, 'h) t -> ('a, 'h) Status.t

    (** Let [t = run_at time f z].  At [time], this runs [f z] and transitions [status t]
        to [Happened h], where [h] is result of [f z].

        More precisely, at [time], provided [abort t a] has not previously been called,
        this will call [f z], with the guarantee that [status t = Scheduled_at time].  If
        [f z] returns [h] and did not call [abort t a], then [status t] becomes [Happened
        h].  If [f z] calls [abort t a], then the result of [f] is ignored, and [status t]
        is [Aborted a].

        If [f z] raises, then [status t] does not transition and remains [Scheduled_at
        time], and the exception is sent to the monitor in effect when [run_at] was
        called. *)
    val run_at : Time.t -> ('z -> 'h) -> 'z -> (_, 'h) t

    val run_after : Time.Span.t -> ('z -> 'h) -> 'z -> (_, 'h) t

    module Abort_result = Time_source.Event.Abort_result

    (** [abort t] changes [status t] to [Aborted] and returns [Ok], unless [t]
        previously happened or was previously aborted. *)
    val abort : ('a, 'h) t -> 'a -> ('a, 'h) Abort_result.t

    (** [abort_exn t a] returns [unit] if [abort t a = `Ok], and otherwise raises. *)
    val abort_exn : ('a, 'h) t -> 'a -> unit

    (** [abort_if_possible t a = ignore (abort t a)]. *)
    val abort_if_possible : ('a, _) t -> 'a -> unit

    module Fired = Time_source.Event.Fired

    val fired : ('a, 'h) t -> ('a, 'h) Fired.t Deferred.t

    module Reschedule_result = Time_source.Event.Reschedule_result

    (** [reschedule_at t] and [reschedule_after t] change the time that [t] will fire, if
        possible, and if not, give a reason why.  Like [run_at], if the requested time is
        in the past, the event will be scheduled to run immediately.  If [reschedule_at t
        time = Ok], then subsequently [scheduled_at t = time].  *)
    val reschedule_at : ('a, 'h) t -> Time.t -> ('a, 'h) Reschedule_result.t

    val reschedule_after : ('a, 'h) t -> Time.Span.t -> ('a, 'h) Reschedule_result.t

    (** [at time]    is [run_at    time ignore ()].
        [after time] is [run_after time ignore ()].

        You should generally prefer to use the [run_*] functions, which allow you to
        synchronously update state via a user-supplied function when the event
        transitions to [Happened].  That is, there is an important difference between:

        {[
          let t = run_at time f () ]}

        and:

        {[
          let t = at time in
          fired t
          >>> function
          | Happened () -> f ()
          | Aborted () -> () ]}

        With [run_at], if [status t = Happened], one knows that [f] has run.  With [at]
        and [fired], one does not know whether [f] has yet run; it may still be scheduled
        to run.  Thus, with [at] and [fired], it is easy to introduce a race.  For
        example, consider these two code snippets:

        {[
          let t = Event.after (sec 2.) in
          upon (Event.fired t) (function
            | Aborted () -> ()
            | Happened () -> printf "Timer fired");
          upon deferred_event (fun () ->
            match Event.abort t () with
            | Ok -> printf "Event occurred"
            | Previously_aborted () -> assert false
            | Previously_happened () -> printf "Event occurred after timer fired"); ]}

        {[
          let t = Event.run_after (sec 2.) printf "Timer fired" in
          upon deferred_event (fun () ->
            match Event.abort t () with
            | Ok -> printf "Event occurred"
            | Previously_aborted () -> assert false
            | Previously_happened () -> printf "Event occurred after timer fired"); ]}

        In both snippets, if [Event.abort] returns [Ok], "Timer fired" is never printed.
        However, the first snippet might print "Event occurred after timer fired" and then
        "Timer fired".  This confused ordering cannot happen with [Event.run_after]. *)
    val at : Time.t -> (_, unit) t

    val after : Time.Span.t -> (_, unit) t
  end

  (** [at_varying_intervals f ?stop] returns a stream whose next element becomes
      determined by calling [f ()] and waiting for that amount of time, and then looping
      to determine subsequent elements.  The stream will end after [stop] becomes
      determined. *)
  val at_varying_intervals
    :  ?stop:unit Deferred.t
    -> (unit -> Time.Span.t)
    -> unit Async_stream.t

  (** [at_intervals interval ?start ?stop] returns a stream whose elements will become
      determined at nonnegative integer multiples of [interval] after the [start] time,
      until [stop] becomes determined:

      {v
        start + 0 * interval
        start + 1 * interval
        start + 2 * interval
        start + 3 * interval
        ...
      v}

      Note that only elements that are strictly in the future ever become determined.
      In particular, if [start] is not in the future, or [start] is not provided,
      then there will be no element before the [interval] has passed.

      If the interval is too small or the CPU is too loaded, [at_intervals] will skip
      until the next upcoming multiple of [interval] after [start]. *)
  val at_intervals
    :  ?start:Time.t
    -> ?stop:unit Deferred.t
    -> Time.Span.t
    -> unit Async_stream.t

  (** [every' ?start ?stop span f] runs [f ()] every [span] amount of time starting when
      [start] becomes determined and stopping when [stop] becomes determined.  [every']
      waits until the result of [f ()] becomes determined before waiting for the next
      [span].

      It is guaranteed that if [stop] becomes determined, even during evaluation of [f],
      then [f] will not be called again by a subsequent iteration of the loop.

      It is an error for [span] to be nonpositive.

      With [~continue_on_error:true], when [f] asynchronously raises, iteration continues.
      With [~continue_on_error:false], if [f] asynchronously raises, then iteration only
      continues when the result of [f] becomes determined.

      Exceptions raised by [f] are always sent to monitor in effect when [every'] was
      called, even with [~continue_on_error:true].

      If [finished] is supplied, [every'] will fill it once all of the following become
      determined: [start], [stop], and the result of the final call to [f]. *)
  val every'
    :  ?start:unit Deferred.t (** default is [return ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> ?finished:unit Ivar.t
    -> Time.Span.t
    -> (unit -> unit Deferred.t)
    -> unit

  (** [every ?start ?stop span f] is
      [every' ?start ?stop span (fun () -> f (); return ())]. *)
  val every
    :  ?start:unit Deferred.t (** default is [return ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> Time.Span.t
    -> (unit -> unit)
    -> unit

  (** [run_at_intervals' ?start ?stop span f] runs [f()] at increments of [start + i *
      span] for nonnegative integers [i], until [stop] becomes determined.
      If the result of [f] is not determined fast enough then the next interval(s)
      are skipped so that there are never multiple concurrent invocations of [f] in
      flight.

      Exceptions raised by [f] are always sent to monitor in effect when
      [run_at_intervals'] was called, even with [~continue_on_error:true]. *)
  val run_at_intervals'
    :  ?start:Time.t (** default is [Time.now ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> Time.Span.t
    -> (unit -> unit Deferred.t)
    -> unit

  (** [run_at_intervals ?start ?stop ?continue_on_error span f] is equivalent to:

      {[
        run_at_intervals' ?start ?stop ?continue_on_error span
          (fun () -> f (); return ()) ]} *)
  val run_at_intervals
    :  ?start:Time.t (** default is [Time.now ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> Time.Span.t
    -> (unit -> unit)
    -> unit
end

(** [Clock_deprecated] is used in [Require_explicit_time_source] to create a clock
    module in which all functions are deprecated. *)
module type Clock_deprecated = sig
  module Time : sig
    module Span : sig
      type t
    end

    type t
  end

  val run_at : Time.t -> ('a -> unit) -> 'a -> unit
  [@@deprecated "[since 2016-02] Use [Time_source]"]

  val run_after : Time.Span.t -> ('a -> unit) -> 'a -> unit
  [@@deprecated "[since 2016-02] Use [Time_source]"]

  val at : Time.t -> unit Deferred.t [@@deprecated "[since 2016-02] Use [Time_source]"]

  val after : Time.Span.t -> unit Deferred.t
  [@@deprecated "[since 2016-02] Use [Time_source]"]

  val with_timeout
    :  Time.Span.t
    -> 'a Deferred.t
    -> [ `Timeout | `Result of 'a ] Deferred.t
  [@@deprecated "[since 2016-02] Use [Time_source]"]

  module Event : sig
    type ('a, 'h) t [@@deriving sexp_of]
    type t_unit = (unit, unit) t [@@deriving sexp_of]

    include
      Invariant.S2 with type ('a, 'b) t := ('a, 'b) t
      [@@deprecated "[since 2016-02] Use [Time_source]"]

    val scheduled_at : (_, _) t -> Time.t
    [@@deprecated "[since 2016-02] Use [Time_source]"]

    module Status : sig
      type ('a, 'h) t =
        | Aborted of 'a
        | Happened of 'h
        | Scheduled_at of Time.t
      [@@deriving sexp_of]
    end

    val status : ('a, 'h) t -> ('a, 'h) Status.t
    [@@deprecated "[since 2016-02] Use [Time_source]"]

    val run_at : Time.t -> ('z -> 'h) -> 'z -> (_, 'h) t
    [@@deprecated "[since 2016-02] Use [Time_source]"]

    val run_after : Time.Span.t -> ('z -> 'h) -> 'z -> (_, 'h) t
    [@@deprecated "[since 2016-02] Use [Time_source]"]

    module Abort_result = Time_source.Event.Abort_result

    val abort : ('a, 'h) t -> 'a -> ('a, 'h) Abort_result.t
    [@@deprecated "[since 2016-02] Use [Time_source]"]

    val abort_exn : ('a, 'h) t -> 'a -> unit
    [@@deprecated "[since 2016-02] Use [Time_source]"]

    val abort_if_possible : ('a, _) t -> 'a -> unit
    [@@deprecated "[since 2016-02] Use [Time_source]"]

    module Fired = Time_source.Event.Fired

    val fired : ('a, 'h) t -> ('a, 'h) Fired.t Deferred.t
    [@@deprecated "[since 2016-02] Use [Time_source]"]

    module Reschedule_result = Time_source.Event.Reschedule_result

    val reschedule_at : ('a, 'h) t -> Time.t -> ('a, 'h) Reschedule_result.t
    [@@deprecated "[since 2016-02] Use [Time_source]"]

    val reschedule_after : ('a, 'h) t -> Time.Span.t -> ('a, 'h) Reschedule_result.t
    [@@deprecated "[since 2016-02] Use [Time_source]"]

    val at : Time.t -> (_, unit) t [@@deprecated "[since 2016-02] Use [Time_source]"]

    val after : Time.Span.t -> (_, unit) t
    [@@deprecated "[since 2016-02] Use [Time_source]"]
  end

  val at_varying_intervals
    :  ?stop:unit Deferred.t
    -> (unit -> Time.Span.t)
    -> unit Async_stream.t
  [@@deprecated "[since 2016-02] Use [Time_source]"]

  val at_intervals
    :  ?start:Time.t
    -> ?stop:unit Deferred.t
    -> Time.Span.t
    -> unit Async_stream.t
  [@@deprecated "[since 2016-02] Use [Time_source]"]

  val every'
    :  ?start:unit Deferred.t (** default is [return ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> ?finished:unit Ivar.t
    -> Time.Span.t
    -> (unit -> unit Deferred.t)
    -> unit
  [@@deprecated "[since 2016-02] Use [Time_source]"]

  val every
    :  ?start:unit Deferred.t (** default is [return ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> Time.Span.t
    -> (unit -> unit)
    -> unit
  [@@deprecated "[since 2016-02] Use [Time_source]"]

  val run_at_intervals'
    :  ?start:Time.t (** default is [Time.now ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> Time.Span.t
    -> (unit -> unit Deferred.t)
    -> unit
  [@@deprecated "[since 2016-02] Use [Time_source]"]

  val run_at_intervals
    :  ?start:Time.t (** default is [Time.now ()] *)
    -> ?stop:unit Deferred.t (** default is [Deferred.never ()] *)
    -> ?continue_on_error:bool (** default is [true] *)
    -> Time.Span.t
    -> (unit -> unit)
    -> unit
  [@@deprecated "[since 2016-02] Use [Time_source]"]
end

(** @inline *)
include (
struct
  [@@@warning "-3"]

  module F1 (C : Clock) : Clock_deprecated = C
  module F2 (C : Clock_deprecated) : Clock = C
end :
sig end)
