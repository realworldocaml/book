(** Internal to Async -- a queue of jobs to run. *)

open! Core_kernel
open! Import
module Scheduler = Scheduler0

type t = Types.Job_queue.t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : unit -> t
val enqueue : t -> Execution_context.t -> ('a -> unit) -> 'a -> unit
val clear : t -> unit
val set_jobs_left_this_cycle : t -> int -> unit
val can_run_a_job : t -> bool
val length : t -> int
val run_jobs : t -> Scheduler.t -> (unit, exn * Backtrace.t) Result.t
val num_jobs_run : t -> int
