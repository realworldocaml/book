open! Import
open Std_internal

module type Span = sig
  (** [t] is immediate on 64bit boxes and so plays nicely with the GC write barrier. *)
  type t = private Int63.t [@@deriving hash]

  include Span_intf.S with type underlying = Int63.t and type t := t

  val of_sec_with_microsecond_precision : float -> t
  val of_int_us : int -> t
  val of_int_ms : int -> t
  val to_int_us : t -> int
  val to_int_ms : t -> int
  val to_int_sec : t -> int

  (** The minimum representable time span. *)
  val min_value_representable : t

  (** The maximum representable time span. *)
  val max_value_representable : t

  (** The minimum span that rounds to a [Time.Span.t] with microsecond precision. *)
  val min_value_for_1us_rounding : t

  (** The maximum span that rounds to a [Time.Span.t] with microsecond precision. *)
  val max_value_for_1us_rounding : t

  (** An alias for [min_value_for_1us_rounding]. *)
  val min_value : t
  [@@deprecated
    "[since 2019-02] use [min_value_representable] or [min_value_for_1us_rounding] \
     instead"]

  (** An alias for [max_value_for_1us_rounding]. *)
  val max_value : t
  [@@deprecated
    "[since 2019-02] use [max_value_representable] or [max_value_for_1us_rounding] \
     instead"]

  (** overflows silently *)
  val scale_int : t -> int -> t

  (** overflows silently *)
  val scale_int63 : t -> Int63.t -> t

  (** Rounds down, and raises unless denominator is positive. *)
  val div : t -> t -> Int63.t

  (** Fast, implemented as the identity function. *)
  val to_int63_ns : t -> Int63.t

  (** Fast, implemented as the identity function. *)
  val of_int63_ns : Int63.t -> t

  (** Will raise on 32-bit platforms.  Consider [to_int63_ns] instead. *)
  val to_int_ns : t -> int

  val of_int_ns : int -> t
  val since_unix_epoch : unit -> t
  val random : ?state:Random.State.t -> unit -> t

  (** WARNING!!! [to_span] and [of_span] both round to the nearest 1us.

      Around 135y magnitudes [to_span] and [of_span] raise.
  *)
  val to_span : t -> Span_float.t
  [@@deprecated
    "[since 2019-01] use [to_span_float_round_nearest] or \
     [to_span_float_round_nearest_microsecond]"]


  val of_span : Span_float.t -> t
  [@@deprecated
    "[since 2019-01] use [of_span_float_round_nearest] or \
     [of_span_float_round_nearest_microsecond]"]

  (** [*_round_nearest] vs [*_round_nearest_microsecond]: If you don't know that you need
      microsecond precision, use the [*_round_nearest] version.
      [*_round_nearest_microsecond] is for historical purposes. *)

  val to_span_float_round_nearest : t -> Span_float.t
  val to_span_float_round_nearest_microsecond : t -> Span_float.t
  val of_span_float_round_nearest : Span_float.t -> t
  val of_span_float_round_nearest_microsecond : Span_float.t -> t

  (** Note that we expose a sexp format that is not the one exposed in [Core]. *)
  module Alternate_sexp : sig
    type nonrec t = t [@@deriving sexp]
  end
  [@@deprecated "[since 2018-04] use [Span.sexp_of_t] and [Span.t_of_sexp] instead"]

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    val of_parts : Parts.t -> t
    val to_parts : t -> Parts.t
  end
end

module type Ofday = sig
  module Span : Span

  (** [t] is immediate on 64bit boxes and so plays nicely with the GC write barrier. *)
  type t = private Int63.t

  (** String and sexp output takes the form 'HH:MM:SS.sssssssss'; see
      {!Core_kernel.Ofday_intf} for accepted input. If input includes more than 9 decimal
      places in seconds, rounds to the nearest nanosecond, with the midpoint rounded up.
      Allows 60[.sss...] seconds for leap seconds but treats it as exactly 60s regardless
      of fractional part. *)
  include
    Ofday_intf.S with type underlying = Int63.t and type t := t and module Span := Span

  (** The largest representable value below [start_of_next_day], i.e. one nanosecond
      before midnight. *)
  val approximate_end_of_day : t

  (*_ This is already exported from [Ofday_intf.S], but we re-declare it to add
    documentation. *)

  (** [add_exn t span] shifts the time of day [t] by [span]. It raises if the result is
      not in the same 24-hour day. Daylight savings shifts are not accounted for. *)
  val add_exn : t -> Span.t -> t


  (** [sub_exn t span] shifts the time of day [t] back by [span]. It raises if the result
      is not in the same 24-hour day. Daylight savings shifts are not accounted for. *)
  val sub_exn : t -> Span.t -> t


  (** [every span ~start ~stop] returns a sorted list of all [t]s that can be expressed as
      [start + (i * span)] without overflow, and satisfying [t >= start && t <= stop].

      If [span <= Span.zero || start > stop], returns an Error.

      The result never crosses the midnight boundary. Constructing a list crossing
      midnight, e.g. every hour from 10pm to 2am, requires multiple calls to [every]. *)
  val every : Span.t -> start:t -> stop:t -> t list Or_error.t
end

(** Time represented as an [Int63.t] number of nanoseconds since the epoch.

    See {!Core.Time_ns} for important user documentation.

    Internally, arithmetic is not overflow-checked. Instead, overflows are silently
    ignored as for [int] arithmetic, unless specifically documented otherwise. Conversions
    may (or may not) raise if prior arithmetic operations overflowed. *)
module type Time_ns = sig
  module Span : Span
  module Ofday : Ofday with module Span := Span

  type t = private Int63.t [@@deriving hash, typerep, bin_io]

  include Comparisons.S with type t := t

  (** Note that we expose a sexp format that is not the one exposed in [Core]. *)
  module Alternate_sexp : sig
    type nonrec t = t [@@deriving sexp]
  end

  include
    Time_intf.Shared with type t := t with module Span := Span with module Ofday := Ofday

  (** Unix epoch (1970-01-01 00:00:00 UTC) *)
  val epoch : t

  (** The minimum representable time. *)
  val min_value_representable : t

  (** The maximum representable time. *)
  val max_value_representable : t

  (** The minimum time that rounds to a [Time.t] with microsecond precision. *)
  val min_value_for_1us_rounding : t

  (** The maximum time that rounds to a [Time.t] with microsecond precision. *)
  val max_value_for_1us_rounding : t

  (** An alias for [min_value_for_1us_rounding]. *)
  val min_value : t
  [@@deprecated
    "[since 2019-02] use [min_value_representable] or [min_value_for_1us_rounding] \
     instead"]

  (** An alias for [max_value_for_1us_rounding]. *)
  val max_value : t
  [@@deprecated
    "[since 2019-02] use [max_value_representable] or [max_value_for_1us_rounding] \
     instead"]

  (** The current time. *)
  val now : unit -> t

  (** overflows silently *)
  val add : t -> Span.t -> t

  (** As [add]; rather than over/underflowing, clamps the result to the closed interval
      between [min_value_representable] and [max_value_representable]. *)
  val add_saturating : t -> Span.t -> t

  (** As [sub]; rather than over/underflowing, clamps the result to the closed interval
      between [min_value_representable] and [max_value_representable]. *)
  val sub_saturating : t -> Span.t -> t

  (** overflows silently *)
  val sub : t -> Span.t -> t

  (** overflows silently *)
  val next : t -> t

  (** overflows silently *)
  val prev : t -> t

  (** overflows silently *)
  val diff : t -> t -> Span.t

  (** overflows silently *)
  val abs_diff : t -> t -> Span.t

  val to_span_since_epoch : t -> Span.t
  val of_span_since_epoch : Span.t -> t
  val to_int63_ns_since_epoch : t -> Int63.t
  val of_int63_ns_since_epoch : Int63.t -> t

  (** Will raise on 32-bit platforms.  Consider [to_int63_ns_since_epoch] instead. *)
  val to_int_ns_since_epoch : t -> int

  val of_int_ns_since_epoch : int -> t

  (** [next_multiple ~base ~after ~interval] returns the smallest [time] of the form:

      {[
        time = base + k * interval
      ]}

      where [k >= 0] and [time > after].  It is an error if [interval <= 0].

      Supplying [~can_equal_after:true] allows the result to satisfy [time >= after].

      Overflows silently. *)
  val next_multiple
    :  ?can_equal_after:bool (** default is [false] *)
    -> base:t
    -> after:t
    -> interval:Span.t
    -> unit
    -> t

  (** [prev_multiple ~base ~before ~interval] returns the largest [time] of the form:

      {[
        time = base + k * interval
      ]}

      where [k >= 0] and [time < before].  It is an error if [interval <= 0].

      Supplying [~can_equal_before:true] allows the result to satisfy [time <= before].
  *)
  val prev_multiple
    :  ?can_equal_before:bool (** default is [false] *)
    -> base:t
    -> before:t
    -> interval:Span.t
    -> unit
    -> t

  val random : ?state:Random.State.t -> unit -> t

  val of_time : Time_float.t -> t
  [@@deprecated
    "[since 2019-01] use [of_time_float_round_nearest] or \
     [of_time_float_round_nearest_microsecond]"]

  val to_time : t -> Time_float.t
  [@@deprecated
    "[since 2019-01] use [to_time_float_round_nearest] or \
     [to_time_float_round_nearest_microsecond]"]

  (** [*_round_nearest] vs [*_round_nearest_microsecond]: If you don't know that you need
      microsecond precision, use the [*_round_nearest] version.
      [*_round_nearest_microsecond] is for historical purposes. *)

  val to_time_float_round_nearest : t -> Time_float.t
  val to_time_float_round_nearest_microsecond : t -> Time_float.t
  val of_time_float_round_nearest : Time_float.t -> t
  val of_time_float_round_nearest_microsecond : Time_float.t -> t

  module Utc : sig
    (** [to_date_and_span_since_start_of_day] computes the date and intraday-offset of a
        time in UTC.  It may be slower than [Core.Time_ns.to_date_ofday], as this
        function does not cache partial results while the latter does. *)
    val to_date_and_span_since_start_of_day : t -> Date0.t * Span.t

    (** The inverse of [to_date_and_span_since_start_of_day]. *)
    val of_date_and_span_since_start_of_day : Date0.t -> Span.t -> t
  end

  module Stable : sig
    module Alternate_sexp : sig
      module V1 : Stable_without_comparator with type t = Alternate_sexp.t
    end

    module Span : sig
      (** [V1] is currently only implemented in [Core]. *)
      module V2 : sig
        type t = Span.t [@@deriving hash]
        type nonrec comparator_witness = Span.comparator_witness

        include
          Stable_int63able
          with type t := t
          with type comparator_witness := comparator_witness

        include
          Comparable.Stable.V1.S
          with type comparable := t
          with type comparator_witness := comparator_witness
      end
    end

    module Ofday : sig
      module V1 : Stable_int63able with type t = Ofday.t
    end
  end
end
