(** Deprecates functions that use wall-clock time, so that code must be explicit about
    what time source is used.  Idiomatic usage is:

    {[
      open! Require_explicit_time_source ]}

    or, in an import.ml:

    {[
      include Require_explicit_time_source ]} *)

open! Core
open! Import
module From_kernel = Async_kernel_require_explicit_time_source

module type Require_explicit_time_source = sig
  (** We shadow [Time_ns] and [Scheduler] from [Async_kernel] because the local versions
      have a different interface.  *)
  include
  module type of struct
    include From_kernel
  end
    with module Scheduler := From_kernel.Scheduler
    with module Time_ns := From_kernel.Time_ns

  module Scheduler : sig
    include module type of struct
    include Scheduler
  end

    val cycle_start : unit -> Time.t [@@deprecated "[since 2016-02] Use [Time_source]"]
  end

  module Time : sig
    include module type of struct
    include Time
  end

    val now : unit -> t [@@deprecated "[since 2016-02] Use [Time_source]"]
  end

  module Time_ns : sig
    include module type of struct
    include Time_ns
  end

    val now : unit -> t [@@deprecated "[since 2016-02] Use [Time_source]"]
  end

  module Clock : Async_kernel.Clock_ns.Clock_deprecated with module Time := Time
end
