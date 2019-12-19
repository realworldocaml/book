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
  include module type of struct
    include From_kernel
  end

  module Scheduler : sig
    include module type of struct
      include Scheduler
    end

    val cycle_start : unit -> Time.t [@@deprecated "[since 2016-02] Use [Time_source]"]
  end

  module Date : sig
    include module type of struct
      include Core.Date
    end

    val today : zone:Time.Zone.t -> t [@@deprecated "[since 2019-05] Use [Time_source]"]
  end

  module Time : sig
    include module type of struct
      include Core.Time
    end

    module Ofday : sig
      include module type of struct
        include Ofday
      end

      val now : zone:Zone.t -> t [@@deprecated "[since 2019-05] Use [Time_source]"]
    end

    val now : unit -> t [@@deprecated "[since 2016-02] Use [Time_source]"]
  end

  module Time_ns : sig
    include module type of struct
      include Core.Time_ns
    end

    module Ofday : sig
      include module type of struct
        include Ofday
      end

      val now : zone:Time.Zone.t -> t [@@deprecated "[since 2019-05] Use [Time_source]"]
    end

    val now : unit -> t [@@deprecated "[since 2016-02] Use [Time_source]"]
  end

  module Clock : Async_kernel.Clock_ns.Clock_deprecated with module Time := Time
end
