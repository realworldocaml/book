(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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

    {!Mtime_clock} provides access to a system monotonic clock. *)

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
  (** See {!Mtime.type-span}. *)

  val zero : span
  (** [zero] is a span of 0ns. *)

  val one : span
  (** [one] is a span of 1ns. *)

  val min_span : span
  (** [min_span] is {!zero}. *)

  val max_span : span
  (** [max_span] is 2{^64}-1ns. *)

  (** {1:preds Predicates} *)

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

  (** {1:const Durations} *)

  val ( * ) : int -> span -> span
  (** [n * dur] is [n] times duration [dur].

      {b Warning.} Does not check for overflow or that [n] is
      positive. *)

  val ns : span
  (** [ns] is a nanosecond duration, 1·10{^-9}s.
      @since 1.4.0 *)

  val us : span
  (** [us] is a microsecond duration, 1·10{^-6}s.
      @since 1.4.0 *)

  val ms : span
  (** [ms] is a millisecond duration, 1·10{^-3}s.
      @since 1.4.0 *)

  val s : span
  (** [s] is a second duration, 1s.
      @since 1.4.0 *)

  val min : span
  (** [min] is a minute duration, 60s.
      @since 1.4.0 *)

  val hour : span
  (** [hour] is an hour duration, 3600s.
      @since 1.4.0 *)

  val day : span
  (** [day] is a day duration, 86'400s.
      @since 1.4.0 *)

  val year : span
  (** [year] is a Julian year duration (365.25 days), 31'557'600s. *)

  (** {1:convert Converting} *)

  val to_uint64_ns : span -> int64
  (** [to_uint64_ns span] is [span] as an {e unsigned} 64-bit integer
      nanosecond span. *)

  val of_uint64_ns : int64 -> span
  (** [of_uint64_ns u] is the {e unsigned} 64-bit integer nanosecond
      span [u] as a span. *)

  val of_float_ns : float -> span option
  (** [of_float_ns f] is the positive floating point nanosecond span [f] as
      a span. This is [None] if [f] is negative, non finite, or
      larger or equal than 2{^53} (~104 days, the largest exact floating point
      integer).
      @since 2.0.0 *)

  val to_float_ns : span -> float
  (** [to_float_ns s] is [span] as a nanosecond floating point span.
      Note that if [s] is larger than 2{^53} (~104 days, the largest
      exact floating point integer) the result is an approximation and
      will not round trip with {!of_float_ns}.
      @since 2.0.0 *)

  (** {1:fmt Formatters} *)

  val pp : Format.formatter -> span -> unit
  (** [pp] formats spans according to their magnitude using SI
      prefixes on seconds and accepted non-SI units. Years are counted
      in Julian years (365.25 SI-accepted days) as
      {{:http://www.iau.org/publications/proceedings_rules/units/}defined}
      by the International Astronomical Union.

      Rounds towards positive infinity, i.e. over approximates, no
      duration is formatted shorter than it is.

      The output is UTF-8 encoded, it uses U+03BC for [µs]
      (10{^-6}[s]). *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf span] formats an unspecified raw representation of [span]
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

val min_stamp : t
(** [min_stamp] is the earliest timestamp. *)

val max_stamp : t
(** [max_stamp] is the latest timestamp. *)

(** {2:preds Predicates} *)

val equal : t -> t -> bool
(** [equal t t'] is [true] iff [t] and [t'] are equal. *)

val compare : t -> t -> int
(** [compare t t'] orders timestamps by increasing time. *)

val is_earlier : t -> than:t -> bool
(** [is_earlier t ~than] is [true] iff [t] occurred before [than]. *)

val is_later : t -> than:t -> bool
(** [is_later t ~than] is [true] iff [t] occurred after [than]. *)

(** {2:arith Arithmetic} *)

val span : t -> t -> span
(** [span t t'] is the span between [t] and [t'] regardless of the
     order between [t] and [t']. *)

val add_span : t -> span -> t option
(** [add_span t s] is the timestamp [s] units later than [t] or [None] if
    the result overflows. *)

val sub_span : t -> span -> t option
(** [sub_span t s] is the timestamp [s] units earlier than [t] or
    [None] if the result underflows. *)

(** {2:fmt Formatting} *)

val pp : Format.formatter -> t -> unit
(** [pp] formats [t] as an {e unsigned} 64-bit integer
    nanosecond timestamp. Note that the absolute value is
    meaningless. *)

val dump : Format.formatter -> t -> unit
(** [dump ppf t] formats an unspecified raw representation of [t] on
    [ppf]. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers

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
