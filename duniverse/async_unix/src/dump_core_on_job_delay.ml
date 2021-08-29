open! Core
open! Import

module Dump_type = struct
  (* This variant mirrors an enum in the C, so order in this declaration matters. *)
  type t =
    | Call_abort
    | Call_gcore
end

external dump_core : Dump_type.t -> unit = "dump_core_on_job_delay_dump_core"
external watch : float -> Dump_type.t -> unit = "dump_core_on_job_delay_watch"
external tick : unit -> unit = "dump_core_on_job_delay_tick"

module How_to_dump = struct
  include Config.Dump_core_on_job_delay.How_to_dump

  let choose_dump_type : t -> Dump_type.t = function
    | Call_abort -> Call_abort
    | Call_gcore -> Call_gcore
    | Default ->
      (match Core.Sys.file_exists "/usr/bin/gcore" with
       | `Yes -> Call_gcore
       | `No | `Unknown -> Call_abort)
  ;;
end

let start_watching ~dump_if_delayed_by ~how_to_dump =
  let dump_type = How_to_dump.choose_dump_type how_to_dump in
  let dump_if_delayed_by_sec = Time.Span.to_sec dump_if_delayed_by in
  let tick_interval = sec (dump_if_delayed_by_sec /. 10.) in
  ignore
    (Thread.create
       ~on_uncaught_exn:`Print_to_stderr
       (fun () -> watch dump_if_delayed_by_sec dump_type)
       ()
     : Thread.t);
  Clock.every tick_interval tick
;;

let dump_core ?(how_to_dump = How_to_dump.Default) () =
  dump_core (How_to_dump.choose_dump_type how_to_dump)
;;
