(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Monotonic time values.

    [Mtime] has platform independent support for monotonic wall-clock
    time. This time increases monotonically and is not subject to
    operating system calendar time adjustments.

    {{!spans}Time spans} represent non-negative monotonic time spans
    between two monotonic clock readings. {{!timestamps}Timestamps}
    represent system-relative monotonic {e timestamps}, their absolute
    value is meaningless but they can be compared across the processes
    of an operating system run.

    {!Mtime_clock} provides access to a system monotonic clock.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:spans Monotonic time spans} *)

type span
(** The type for non-negative monotonic time spans. They represent the
    difference between two monotonic clock readings. If the platform's
    clock has nanosecond resolution the representation guarantees that
    the function {!Mtime_clock.elapsed} can measure up to
    approximatively 584 Julian year spans before silently rolling over
    (unlikely since this is in a single program run). *)

(** Monotonic time spans. *)
module Span : sig

  (** {1:spans Monotonic time spans} *)

  type t = span
  (** See {!type:span}. *)

  val to_uint64_ns : span -> int64
  (** [to_uint64_ns span] is [span] as an {e unsigned} 64-bit integer
      nanosecond span. *)

  val of_uint64_ns : int64 -> span
  (** [of_uint64_ns d] is the {e unsigned} 64-bit integer nanosecond
      span as a span. *)

  (** {1 Constants} *)

  val zero : span
  (** [zero] is a span of 0ns. *)

  val one : span
  (** [one] is a span of 1ns. *)

  val min_span : span
  (** [min_span] is {!zero}. *)

  val max_span : span
  (** [max_span] is 2^64-1ns. *)

  (** {1 Predicates} *)

  val equal : span -> span -> bool
  (** [equal span span'] is [true] iff [span] and [span'] are equal. *)

  val compare : span -> span -> int
  (** [compare span span'] orders spans by increasing duration. *)

  (** {1:arith Arithmetic} *)

  val add : span -> span -> span
  (** [add span span'] is [span + span'].

      {b Warning.} Rolls over on overflow. *)

  val abs_diff : span -> span -> span
  (** [abs_diff span span'] is the absolute difference between
      [span] and [span']. *)

  (** {1 Converting time spans}

      See {{!convert}this section} for time scale definitions.  *)

  val to_ns : span -> float
  (** [to_ns span] is [span] in nanoseconds (1e-9s). *)

  val to_us : span -> float
  (** [to_us span] is [span] in microseconds (1e-6s). *)

  val to_ms : span -> float
  (** [to_ms span] is [span] in milliseconds (1e-3s). *)

  val to_s : span -> float
  (** [to_s span] is [span] is seconds. *)

  val to_min : span -> float
  (** [to_min span] is [span] in SI-accepted minutes (60s). *)

  val to_hour : span -> float
  (** [to_hour span] is [span] in SI-accepted hours (3600s). *)

  val to_day : span -> float
  (** [to_day span] is [span] in SI-accepted days (24 hours, 86400s). *)

  val to_year : span -> float
  (** [to_year span] is [span] in Julian years (365.25 days, 31'557'600s). *)

  (** {1 Pretty printing} *)

  val pp : Format.formatter -> span -> unit
  (** [pp_span ppf span] prints an unspecified representation of
      [span] on [ppf]. The representation is not fixed-width,
      depends on the magnitude of [span] and uses locale
      independent {{!convert}standard time scale} abbreviations. *)

  val pp_float_s : Format.formatter -> float -> unit
  (** [pp_float_s] prints like {!pp} does but on a floating
      point seconds time span value (which can be negative). *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf span] prints an unspecified raw representation of [span]
      on [ppf]. *)

  (**/**)

  val unsafe_of_uint64_ns_option : int64 option -> t option
end

(** {1:timestamps Monotonic timestamps}

    {b Note.} Only use timestamps if you need inter-process time
    correlation, otherwise prefer {!Mtime_clock.elapsed} and
    {{!Mtime_clock.counters}counters}. *)

type t
(** The type for monotonic timestamps relative to an indeterminate
    system-wide event (e.g. last startup). Their absolute value has no
    meaning but can be used for inter-process time correlation. *)

val to_uint64_ns : t -> int64
(** [to_uint64_ns t] is [t] as an {e unsigned} 64-bit integer
     nanosecond timestamp. The absolute value is meaningless. *)

val of_uint64_ns : int64 -> t
(** [to_uint64_ns t] is [t] is an {e unsigned} 64-bit integer
    nanosecond timestamp as a timestamp.

    {b Warning.} Timestamps returned by this function should only be
    used with other timestamp values that are know to come from the
    same operating system run. *)

(** {1:preds Predicates} *)

val equal : t -> t -> bool
(** [equal t t'] is [true] iff [t] and [t'] are equal. *)

val compare : t -> t -> int
(** [compare t t'] orders timestamps by increasing time. *)

val is_earlier : t -> than:t -> bool
(** [is_earlier t ~than] is [true] iff [t] occurred before [than]. *)

val is_later : t -> than:t -> bool
(** [is_later t ~than] is [true] iff [t] occurred after [than]. *)

(** {1:arith Arithmetic} *)

val span : t -> t -> span
(** [span t t'] is the span between [t] and [t'] regardless of the
     order between [t] and [t']. *)

val add_span : t -> span -> t option
(** [add_span t s] is the timestamp [s] units later than [t] or [None] if
    the result overflows. *)

val sub_span : t -> span -> t option
(** [sub_span t s] is the timestamp [s] units earlier than [t] or
    [None] if overflows. *)

(** {1:pretty Pretty printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints [t] as an {e unsigned} 64-bit integer nanosecond
    timestamp. Note that the absolute value is meaningless. *)

val dump : Format.formatter -> t -> unit
(** [dump ppf t] prints an unspecified raw representation of [t] on [ppf]. *)

(** {1:convert Time scale conversion}

    The following convenience constants relate time scales to seconds.
    Used as multiplicands they can be used to convert these units
    to and from seconds.

    The constants are defined according to
    {{:http://www.bipm.org/en/publications/si-brochure/chapter3.html}SI
    prefixes} on seconds and
    {{:http://www.bipm.org/en/publications/si-brochure/table6.html}accepted
    non-SI units}. Years are counted in Julian years (365.25 SI-accepted days)
    as {{:http://www.iau.org/publications/proceedings_rules/units/}defined}
    by the International Astronomical Union (IAU). *)

val ns_to_s : float
(** [ns_to_s] is [1e-9] the number of seconds in one nanosecond. *)

val us_to_s : float
(** [us_to_s] is [1e-6], the number of seconds in one microsecond. *)

val ms_to_s : float
(** [ms_to_s] is [1e-3], the number of seconds in one millisecond. *)

val min_to_s : float
(** [min_to_s] is [60.], the number of seconds in one SI-accepted minute. *)

val hour_to_s : float
(** [hour_to_s] is [3600.], the number of seconds in one SI-accepted hour. *)

val day_to_s : float
(** [day_to_s] is [86_400.], the number of seconds in one SI-accepted day. *)

val year_to_s : float
(** [year_to_s] is [31_557_600.], the number of seconds in a Julian year. *)

val s_to_ns : float
(** [s_to_ns] is [1e9] the number of nanoseconds in one second. *)

val s_to_us : float
(** [s_to_us] is [1e6], the number of microseconds in one second. *)

val s_to_ms : float
(** [s_to_ms] is [1e3], the number of milliseconds in one second. *)

val s_to_min : float
(** [s_to_min] is [1. /. 60.], the number of SI-accepted minutes in
    one second.  *)

val s_to_hour : float
(** [s_to_hour] is [1. /. 3600.], the number of SI-accepted hours in
    one second. *)

val s_to_day : float
(** [s_to_day] is [1. /. 86400.], the number of SI-accepted days in
    one second. *)

val s_to_year : float
(** [s_to_year] is [1. /. 31_557_600.], the number of Julian years
    in one second. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
