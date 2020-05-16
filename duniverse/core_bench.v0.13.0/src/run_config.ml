(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open Core

type t = {
  verbosity:Verbosity.t;
  no_compactions:bool;
  quota:Quota.t;
  sampling_type:[`Geometric of float | `Linear of int];
  stabilize_gc_between_runs:bool;
  fork_each_benchmark:bool;
  thin_overhead:int option
} [@@deriving fields, sexp]

let create
    ?(verbosity=Verbosity.Low)
    ?(no_compactions=Defaults.no_compactions)
    ?(quota=Defaults.quota)
    ?(sampling_type=`Geometric Defaults.geometric_scale)
    ?(stabilize_gc_between_runs=Defaults.stabilize_gc_between_runs)
    ?(fork_each_benchmark=Defaults.fork_each_benchmark)
    ?thin_overhead
    ()
  =
  { verbosity;
    no_compactions;
    quota;
    sampling_type;
    stabilize_gc_between_runs;
    fork_each_benchmark;
    thin_overhead;
  }
