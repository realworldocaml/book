(** Deprecates functions that use wall-clock time, so that code must be explicit about
    what time source is used.

    Idiomatic usage is:

    {[
      open! Require_explicit_time_source ]}

    or, in an import.ml:

    {[
      include Require_explicit_time_source ]} *)

open! Core_kernel
open! Import
module Clock_ns : Clock_intf.Clock_deprecated with module Time := Time_ns

module Time_ns : sig
  include module type of struct
    include Time_ns
  end

  val now : unit -> t [@@deprecated "[since 2016-02] Use [Time_source]"]
end

module Time : sig
  include module type of struct
    include Time
  end

  val now : unit -> t [@@deprecated "[since 2019-05] Use [Time_source]"]
end

module Async_kernel_scheduler : sig
  include module type of struct
    include Async_kernel_scheduler
  end

  val cycle_start : unit -> Time.t [@@deprecated "[since 2016-02] Use [Time_source]"]

  val cycle_start_ns : unit -> Time_ns.t
  [@@deprecated "[since 2016-02] Use [Time_source]"]
end

module Date : sig
  include module type of struct
    include Date
  end

  val today : zone:Time.Zone.t -> t [@@deprecated "[since 2019-05] Use [Time_source]"]
end

val at : Time_ns.t -> unit Deferred.t [@@deprecated "[since 2016-02] Use [Time_source]"]

val after : Time_ns.Span.t -> unit Deferred.t
[@@deprecated "[since 2016-02] Use [Time_source]"]

val every
  :  ?start:unit Deferred.t
  -> ?stop:unit Deferred.t
  -> ?continue_on_error:bool
  -> Time_ns.Span.t
  -> (unit -> unit)
  -> unit
[@@deprecated "[since 2016-02] Use [Time_source]"]

val with_timeout
  :  Time_ns.Span.t
  -> 'a Deferred.t
  -> [ `Timeout | `Result of 'a ] Deferred.t
[@@deprecated "[since 2016-02] Use [Time_source]"]
