(** A specialized priority queue for a set of time-based alarms.

    A timing wheel is a data structure that maintains a clock with the current time and a
    set of alarms scheduled to fire in the future.  One can add and remove alarms, and
    advance the clock to cause alarms to fire.  There is nothing asynchronous about a
    timing wheel.  Alarms only fire in response to an [advance_clock] call.

    When one [create]s a timing wheel, one supplies an initial time, [start], and an
    [alarm_precision].  The timing wheel breaks all time from the epoch onwards into
    half-open intervals of size [alarm_precision], with the bottom half of each interval
    closed, and the top half open.  Alarms in the same interval fire in the same call to
    [advance_clock], as soon as [now t] is greater than all the times in the interval.
    When an alarm [a] fires on a timing wheel [t], the implementation guarantees that:

    {[
      Alarm.at a < now t
    ]}

    That is, alarms never fire early.  Furthermore, the implementation guarantees that
    alarms don't go off too late.  More precisely, for all alarms [a] in [t]:

    {[
      interval_start t (Alarm.at a) >= interval_start t (now t)
    ]}

    This implies that for all alarms [a] in [t]:

    {[
      Alarm.at a > now t - alarm_precision t
    ]}

    Of course, an [advance_clock] call can advance the clock to an arbitrary time in the
    future, and thus alarms may fire at a clock time arbitrarily far beyond the time for
    which they were set.  But the implementation has no control over the times supplied to
    [advance_clock]; it can only guarantee that alarms will fire when [advance_clock] is
    called with a time at least [alarm_precision] greater than their scheduled time.

    {2 Implementation}

    A timing wheel is implemented using a specialized priority queue in which the
    half-open intervals from the epoch onwards are numbered 0, 1, 2, etc.  Each time is
    stored in the priority queue with the key of its interval number.  Thus all alarms
    with a time in the same interval get the same key, and hence fire at the same
    time. More specifically, an alarm is fired when the clock reaches or passes the time
    at the start of the next interval.

    Alarms that fire in the same interval will fire in the order in which they were added
    to the timing wheel, rather than the time they were set to go off.  This is consistent
    with the guarantees of timing wheel mentioned above, but may nontheless be surprising
    to users.

    The priority queue is implemented with an array of levels of decreasing precision,
    with the lowest level having the most precision and storing the closest upcoming
    alarms, while the highest level has the least precision and stores the alarms farthest
    in the future.  As time increases, the timing wheel does a lazy radix sort of the
    alarm keys.

    This implementation makes [add_alarm] and [remove_alarm] constant time, while
    [advance_clock] takes time proportional to the amount of time the clock is advanced.
    With a sufficient number of alarms, this is more efficient than a log(N) heap
    implementation of a priority queue.

    {2 Representable times}

    A timing wheel [t] can only handle a (typically large) bounded range of times as
    determined by the current time, [now t], and the [level_bits] and [alarm_precision]
    arguments supplied to [create].  Various functions raise if they are supplied a time
    smaller than [now t] or [> max_allowed_alarm_time t].  This situation likely indicates
    a misconfiguration of the [level_bits] and/or [alarm_precision].  Here is the duration
    of [max_allowed_alarm_time t - now t] using the default [level_bits].

    {v
      | # intervals | alarm_precision | duration |
      +-------------+-----------------+----------|
      |        2^61 | nanosecond      | 73 years |
    v} *)

open! Core_kernel
open! Import

(** An [Interval_num.t] is an index of one of the intervals into which a timing-wheel
    partitions time. *)
module type Interval_num = sig
  module Span : sig
    type t = private Int63.t [@@deriving sexp_of]

    include Comparable.S with type t := t

    val max : t -> t -> t
    val zero : t
    val one : t
    val max_value : t
    val of_int63 : Int63.t -> t
    val to_int63 : t -> Int63.t
    val of_int : int -> t
    val to_int_exn : t -> int
    val scale_int : t -> int -> t
    val pred : t -> t
    val succ : t -> t
    val ( + ) : t -> t -> t
  end

  type t = private Int63.t [@@deriving sexp_of]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val max : t -> t -> t
  val min : t -> t -> t
  val zero : t
  val one : t
  val min_value : t
  val max_value : t
  val of_int63 : Int63.t -> t
  val to_int63 : t -> Int63.t
  val of_int : int -> t
  val to_int_exn : t -> int
  val add : t -> Span.t -> t
  val sub : t -> Span.t -> t
  val diff : t -> t -> Span.t
  val succ : t -> t
  val pred : t -> t
  val rem : t -> Span.t -> Span.t
end

(** An [Alarm_precision] is a time span that is a power of two number of nanoseconds, used
    to specify the precision of a timing wheel. *)
module type Alarm_precision = sig
  type t [@@deriving compare, sexp_of]

  include Equal.S with type t := t

  val of_span : Time_ns.Span.t -> t
  [@@deprecated "[since 2018-01] Use [of_span_floor_pow2_ns]"]

  (** [of_span_floor_pow2_ns span] returns the largest alarm precision less than or equal
      to [span] that is a power of two number of nanoseconds. *)
  val of_span_floor_pow2_ns : Time_ns.Span.t -> t

  val to_span : t -> Time_ns.Span.t
  val one_nanosecond : t

  (** Constants that are the closest power of two number of nanoseconds to the stated
      span. *)

  (** ~19.5 h  *)
  val about_one_day : t

  (** 1024 us *)
  val about_one_microsecond : t

  (** ~1.05 ms *)
  val about_one_millisecond : t

  (** ~1.07 s  *)
  val about_one_second : t

  (** [mul t ~pow2] is [t * 2^pow2].  [pow2] may be negative, but [mul] does not check for
      overflow or underflow. *)
  val mul : t -> pow2:int -> t

  (** [div t ~pow2] is [t / 2^pow2].  [pow2] may be negative, but [div] does not check
      for overflow or underflow. *)
  val div : t -> pow2:int -> t

  (** The unstable bin and sexp format is that of [Time_ns.Span], with the caveat that
      deserialization implicitly floors the time span to the nearest power of two
      nanoseconds.  This ensures that the alarm precision that is used is at least as
      precise than the alarm precision that is stated. *)
  module Unstable : sig
    type nonrec t = t [@@deriving bin_io, compare, sexp]
  end
end

(** A timing wheel can be thought of as a set of alarms. *)
module type Timing_wheel = sig
  module Alarm_precision : Alarm_precision

  type 'a t [@@deriving sexp_of]
  type 'a timing_wheel = 'a t

  (** [<:sexp_of< _ t_now >>] displays only [now t], not all the alarms. *)
  type 'a t_now = 'a t [@@deriving sexp_of]

  module Interval_num : Interval_num

  module Alarm : sig
    type 'a t [@@deriving sexp_of]

    (** [null ()] returns an alarm [t] such that [not (mem timing_wheel t)] for all
        [timing_wheel]s. *)
    val null : unit -> _ t

    (** All [Alarm] functions will raise if [not (Timing_wheel.mem timing_wheel t)]. *)
    val at : 'a timing_wheel -> 'a t -> Time_ns.t

    val interval_num : 'a timing_wheel -> 'a t -> Interval_num.t
    val value : 'a timing_wheel -> 'a t -> 'a
  end

  include Invariant.S1 with type 'a t := 'a t

  module Level_bits : sig
    (** The timing-wheel implementation uses an array of "levels", where level [i] is an
        array of length [2^b_i], where the [b_i] are the "level bits" specified via
        [Level_bits.create_exn [b_0, b_1; ...]].

        A timing wheel can handle approximately [2 ** num_bits t] intervals/keys beyond
        the current minimum time/key, where [num_bits t = b_0 + b_1 + ...].

        One can use a [Level_bits.t] to trade off run time and space usage of a timing
        wheel.  For a fixed [num_bits], as the number of levels increases, the length of
        the levels decreases and the timing wheel uses less space, but the constant factor
        for the running time of [add] and [increase_min_allowed_key] increases. *)
    type t [@@deriving sexp]

    include Invariant.S with type t := t

    (** [max_num_bits] is how many bits in a key the timing wheel can use, i.e. 61.  We
        subtract 3 for the bits in the word that we won't use:

        - for the tag bit
        - for negative numbers
        - so we can do arithmetic around the bound without worrying about overflow *)
    val max_num_bits : int

    (** In [create_exn bits], it is an error if any of the [b_i] in [bits] has [b_i <= 0],
        or if the sum of the [b_i] in [bits] is greater than [max_num_bits].  With
        [~extend_to_max_num_bits:true], the resulting [t] is extended with sufficient [b_i
        = 1] so that [num_bits t = max_num_bits]. *)
    val create_exn
      :  ?extend_to_max_num_bits:bool (** default is [false] *)
      -> int list
      -> t

    (** [default] returns the default value of [level_bits] used by [Timing_wheel.create]
        and [Timing_wheel.Priority_queue.create].

        {[
          default = [11; 10; 10; 10; 10; 10]
        ]}

        This default uses 61 bits, i.e. [max_num_bits], and less than 10k words of
        memory. *)
    val default : t

    (** [num_bits t] is the sum of the [b_i] in [t]. *)
    val num_bits : t -> int
  end

  module Config : sig
    type t [@@deriving sexp]

    include Invariant.S with type t := t

    (** [create] raises if [alarm_precision <= 0]. *)
    val create
      :  ?capacity:int (** default is [1] *)
      -> ?level_bits:Level_bits.t
      -> alarm_precision:Alarm_precision.t
      -> unit
      -> t

    (** accessors *)
    val alarm_precision : t -> Time_ns.Span.t

    val level_bits : t -> Level_bits.t

    (** [durations t] returns the durations of the levels in [t] *)
    val durations : t -> Time_ns.Span.t list

    (** [microsecond_precision ()] returns a reasonable configuration for a timing wheel
        with microsecond [alarm_precision], and level durations of 1ms, 1s, 1m, 1h, 1d.
        See the relevant expect test in [Core_kernel_test] library. *)
    val microsecond_precision : unit -> t
  end

  (** [create ~config ~start] creates a new timing wheel with current time [start].
      [create] raises if [start < Time_ns.epoch].  For a fixed [level_bits], a smaller
      (i.e. more precise) [alarm_precision] decreases the representable range of
      times/keys and increases the constant factor for [advance_clock]. *)
  val create : config:Config.t -> start:Time_ns.t -> 'a t

  (** Accessors *)
  val alarm_precision : _ t -> Time_ns.Span.t

  val now : _ t -> Time_ns.t
  val start : _ t -> Time_ns.t

  (** One can think of a timing wheel as a set of alarms.  Here are various container
      functions along those lines. *)

  val is_empty : _ t -> bool
  val length : _ t -> int
  val iter : 'a t -> f:('a Alarm.t -> unit) -> unit

  (** [interval_num t time] returns the number of the interval that [time] is in, where
      [0] is the interval that starts at [Time_ns.epoch].  [interval_num] raises if
      [Time_ns.( < ) time Time_ns.epoch]. *)
  val interval_num : _ t -> Time_ns.t -> Interval_num.t

  (** [now_interval_num t = interval_num t (now t)]. *)
  val now_interval_num : _ t -> Interval_num.t

  (** [interval_num_start t n] is the start of the [n]'th interval in [t], i.e.
      [n * alarm_precision t] after the epoch.

      [interval_start t time] is the start of the half-open interval containing [time],
      i.e.:

      {[
        interval_num_start t (interval_num t time)
      ]} *)
  val interval_num_start : _ t -> Interval_num.t -> Time_ns.t

  (** [interval_start] raises in the same cases that [interval_num] does. *)
  val interval_start : _ t -> Time_ns.t -> Time_ns.t


  (** [advance_clock t ~to_ ~handle_fired] advances [t]'s clock to [to_].  It fires and
      removes all alarms [a] in [t] with [Time_ns.(<) (Alarm.at t a) (interval_start t
      to_)], applying [handle_fired] to each such [a].

      If [to_ <= now t], then [advance_clock] does nothing.

      [advance_clock] fails if [to_] is too far in the future to represent.

      Behavior is unspecified if [handle_fired] accesses [t] in any way other than
      [Alarm] functions. *)
  val advance_clock : 'a t -> to_:Time_ns.t -> handle_fired:('a Alarm.t -> unit) -> unit

  (** [fire_past_alarms t ~handle_fired] fires and removes all alarms [a] in [t] with
      [Time_ns.( <= ) (Alarm.at t a) (now t)], applying [handle_fired] to each such [a].

      [fire_past_alarms] visits all alarms in interval [now_interval_num], to check their
      [Alarm.at].

      Behavior is unspecified if [handle_fired] accesses [t] in any way other than
      [Alarm] functions. *)
  val fire_past_alarms : 'a t -> handle_fired:('a Alarm.t -> unit) -> unit

  (** [max_allowed_alarm_time t] returns the greatest [at] that can be supplied to [add].
      [max_allowed_alarm_time] is not constant; its value increases as [now t]
      increases. *)
  val max_allowed_alarm_time : _ t -> Time_ns.t

  (** [min_allowed_alarm_interval_num t = now_interval_num t] *)
  val min_allowed_alarm_interval_num : _ t -> Interval_num.t

  (** [max_allowed_alarm_interval_num t = interval_num t (max_allowed_alarm_time t)] *)
  val max_allowed_alarm_interval_num : _ t -> Interval_num.t

  (** [add t ~at a] adds a new value [a] to [t] and returns an alarm that can later be
      supplied to [remove] the alarm from [t].  [add] raises if [interval_num t at <
      now_interval_num t || at > max_allowed_alarm_time t]. *)
  val add : 'a t -> at:Time_ns.t -> 'a -> 'a Alarm.t

  (** [add_at_interval_num t ~at a] is equivalent to [add t ~at:(interval_num_start t at)
      a]. *)
  val add_at_interval_num : 'a t -> at:Interval_num.t -> 'a -> 'a Alarm.t

  val mem : 'a t -> 'a Alarm.t -> bool

  (** [remove t alarm] removes [alarm] from [t].  [remove] raises if [not (mem t
      alarm)]. *)
  val remove : 'a t -> 'a Alarm.t -> unit

  (** [reschedule t alarm ~at] mutates [alarm] so that it will fire at [at], i.e. so that
      [Alarm.at t alarm = at].  [reschedule] raises if [not (mem t alarm)] or if [at] is
      an invalid time for [t], in the same situations that [add] raises. *)
  val reschedule : 'a t -> 'a Alarm.t -> at:Time_ns.t -> unit

  (** [reschedule_at_interval_num t alarm ~at] is equivalent to:
      {[
        reschedule t alarm ~at:(interval_num_start t at)
      ]} *)
  val reschedule_at_interval_num : 'a t -> 'a Alarm.t -> at:Interval_num.t -> unit

  (** [clear t] removes all alarms from [t]. *)
  val clear : _ t -> unit

  (** [min_alarm_interval_num t] is the minimum [Alarm.interval_num] of all alarms in
      [t]. *)
  val min_alarm_interval_num : _ t -> Interval_num.t option

  (** [min_alarm_interval_num_exn t] is like [min_alarm_interval_num], except it raises if
      [is_empty t]. *)
  val min_alarm_interval_num_exn : _ t -> Interval_num.t

  (** [max_alarm_time_in_min_interval t] returns the maximum [Alarm.at] over all alarms in
      [t] whose [Alarm.interval_num] is [min_alarm_interval_num t].  This function is
      useful for advancing to the [min_alarm_interval_num] of a timing wheel and then
      calling [fire_past_alarms] to fire the alarms in that interval.  That is useful when
      simulating time, to ensure that alarms are processed in order. *)
  val max_alarm_time_in_min_interval : 'a t -> Time_ns.t option

  (** [max_alarm_time_in_min_interval_exn t] is like [max_alarm_time_in_min_interval],
      except that it raises if [is_empty t]. *)
  val max_alarm_time_in_min_interval_exn : 'a t -> Time_ns.t

  (** [next_alarm_fires_at t] returns the minimum time to which the clock can be advanced
      such that an alarm will fire, or [None] if [t] has no alarms (or all alarms
      are in the max interval, and hence cannot fire).  If
      [next_alarm_fires_at t = Some next], then for the minimum alarm time [min] that
      occurs in [t], it is guaranteed that: [next - alarm_precision t <= min < next]. *)
  val next_alarm_fires_at : _ t -> Time_ns.t option

  (** [next_alarm_fires_at_exn] is like [next_alarm_fires_at], except that it raises if
      [is_empty t]. *)
  val next_alarm_fires_at_exn : _ t -> Time_ns.t

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    val max_time : Time_ns.t

    val interval_num_internal
      :  time:Time_ns.t
      -> alarm_precision:Alarm_precision.t
      -> Interval_num.t

    module Num_key_bits : sig
      type t

      include Invariant.S with type t := t

      val zero : t
    end
  end
end
