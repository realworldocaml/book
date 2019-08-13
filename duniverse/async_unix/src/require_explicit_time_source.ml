open! Core
open! Import
include Require_explicit_time_source_intf

include (
  From_kernel :
    module type of struct
    include From_kernel
  end
  with module Scheduler := From_kernel.Scheduler
  with module Time_ns := From_kernel.Time_ns)

module Scheduler = Scheduler
module Time = Time
module Time_ns = Time_ns
module Clock = Clock
