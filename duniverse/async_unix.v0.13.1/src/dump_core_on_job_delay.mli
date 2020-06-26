(** Dump core if jobs are delayed, to get additional debug information when running on
    UNIX systems that support core dumps.

    It is not normally enabled, but may be enabled for any program by setting the
    appropriate field, [dump_core_on_job_delay], in the [ASYNC_CONFIG] environment
    variable. *)

open! Core
open! Import

module How_to_dump : sig
  type t = Config.Dump_core_on_job_delay.How_to_dump.t =
    | Default
    | Call_abort
    | Call_gcore
  [@@deriving sexp_of]
end

(** [start_watching] starts a regular async job (via [Clock.every]) that increments a
    counter, and a C thread to make sure that the counter is incremented in a timely
    manner. *)
val start_watching : dump_if_delayed_by:Time.Span.t -> how_to_dump:How_to_dump.t -> unit

(** [dump_core ()] dumps a core file using [/usr/bin/gcore] if it exists, or by calling
    [abort()] if not (or with [~how_to_dump:Call_abort]).  With gcore, the dump is done in
    a child process, and the core file is written to [/tmp/core.$N.$PID], where [$PID] is
    the process id and [$N] is a counter that is incremented on each call to
    [dump_core]. *)
val dump_core : ?how_to_dump:How_to_dump.t -> unit -> unit
