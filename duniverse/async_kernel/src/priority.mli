(** A job's priority determines when in an async cycle the job is run. A "cycle" runs a
    bounded number of [Normal] priority jobs followed by a bounded number of [Low]
    priority jobs. The bound is [Async_kernel_config.max_num_jobs_per_priority_per_cycle].

    The primary consequence and use case of [Low] priority is to improve batching in jobs
    that flush buffers filled by other, [Normal] priority jobs.

    Neither priority can starve out the other entirely. If there are more [Normal]
    priority jobs than can run in a single cycle, then the rest will be delayed until
    after (some) [Low] priority jobs run.
*)

open! Core
open! Import

type t =
  | Normal
  | Low
[@@deriving sexp_of]

val normal : t
val low : t
