module Async_config = Config
module Clock = Clock
module Dump_core_on_job_delay = Dump_core_on_job_delay
module Fd = Fd
module In_thread = In_thread
module Io_stats = Io_stats
module Log = Log
module Print = Async_print
module Process = Process
module Reader = Reader
module Require_explicit_time_source = Require_explicit_time_source
module Scheduler = Scheduler
module Shutdown = Shutdown
module Signal = Signal
module Socket = Unix_syscalls.Socket
module Sys = Async_sys
module Tcp = Tcp
module Thread_safe = Thread_safe
module Writer = Writer

module Unix = struct
  module Fd = Fd

  include Unix_syscalls (** @open *)
end

(* see comment in assign_try_with_log_exn.mli *)
include Assign_try_with_log_exn

let after = Clock.after
let at = Clock.at
let every = Clock.every
let with_timeout = Clock.with_timeout
let schedule = Scheduler.schedule
let schedule' = Scheduler.schedule'
let shutdown = Shutdown.shutdown
let within = Scheduler.within
let within' = Scheduler.within'

(* We rebind all pervasive and some Core funtions that deal with I/O so that one
   doesn't unintentionally do blocking stuff in an Async program. *)

(** Shadow blocking functions in [Core.Printf] to prevent their unintentional use. *)
module Printf = struct
  let _shadow = `Probably_should_not_use_blocking_Core_Printf_functions_with_Async
  let bprintf = Core.Printf.bprintf
  let eprintf = _shadow
  let exitf = _shadow
  let failwithf = Core.Printf.failwithf
  let fprintf _ = _shadow
  let ifprintf _ = Core.Printf.ifprintf
  let invalid_argf = Core.Printf.invalid_argf
  let kbprintf = Core.Printf.kbprintf
  let kfprintf _ _ = _shadow
  let ksprintf = Core.Printf.ksprintf
  let printf = _shadow
  let sprintf = Core.Printf.sprintf
end

include struct
  open Core

  module Overwrite_ = struct
    let overwrite1 (`This_is_async__Think_about_blocking as x) = x
    let overwrite2 `This_is_async__Think_about_blocking = overwrite1
    let overwrite3 `This_is_async__Think_about_blocking = overwrite2
    let overwrite4 `This_is_async__Think_about_blocking = overwrite3
  end

  open Overwrite_

  let close_in_noerr = overwrite1
  let close_in = overwrite1
  let close_out_noerr = overwrite1
  let close_out = overwrite1
  let eprintf = Print.eprintf
  let flush_all = overwrite1
  let flush = overwrite1
  let fprintf = Print.fprintf
  let ifprintf = Printf.ifprintf
  let in_channel_length = overwrite1
  let input_binary_int = overwrite1
  let input_byte = overwrite1
  let input_char = overwrite1
  let input_line = overwrite1
  let input_lines ?fix_win_eol:_ = overwrite1
  let input = overwrite4
  let input_value = overwrite1
  let open_in_bin = overwrite1
  let open_in_gen = overwrite3
  let open_in = overwrite1
  let open_out_bin = overwrite1
  let open_out_gen = overwrite3
  let open_out = overwrite1
  let out_channel_length = overwrite1
  let output_binary_int = overwrite2
  let output_byte = overwrite2
  let output_char = overwrite2
  let output = overwrite4
  let output_string = overwrite2
  let output_value = overwrite2
  let pos_in = overwrite1
  let pos_out = overwrite1
  let prerr_char = Print.prerr_char
  let prerr_endline = Print.prerr_endline
  let prerr_float = Print.prerr_float
  let prerr_int = Print.prerr_int
  let prerr_newline = Print.prerr_newline
  let prerr_string = Print.prerr_string
  let print_char = Print.print_char
  let print_endline = Print.print_endline
  let print_float = Print.print_float
  let printf = Print.printf
  let print_int = Print.print_int
  let print_newline = Print.print_newline
  let print_s = Print.print_s
  let print_string = Print.print_string
  let read_float = overwrite1
  let read_int = overwrite1
  let read_line = overwrite1
  let read_lines = overwrite1
  let read_wrap ?binary:_ ~f:_ = overwrite1
  let really_input = overwrite4
  let seek_in = overwrite2
  let seek_out = overwrite1
  let set_binary_mode_in = overwrite2
  let set_binary_mode_out = overwrite2
  let write_lines = overwrite2
  let write_wrap ?binary:_ ~f:_ = overwrite1

  let eprint_s =
    overwrite1
  [@@deprecated
    "[since 2019-12] If you want to the blocking version, use [Core.eprint_s] (this \
     preserves behavior, but is discouraged). If you want the nonblocking version, \
     use [eprint_s_nonblocking] or [Print.eprint_s]"]
  ;;

  let eprint_s_nonblocking = Print.eprint_s

  module LargeFile = struct
    let seek_out = overwrite1
    let pos_out = overwrite1
    let out_channel_length = overwrite1
    let seek_in = overwrite1
    let pos_in = overwrite1
    let in_channel_length = overwrite1
  end

  module Sexp : sig
  include module type of struct
    include Sexp
  end


  val save : ?perm:int -> string -> t -> unit
  [@@alert blocking "Use [Writer.save_sexp ~hum:false] to avoid blocking."]

  val save_hum : ?perm:int -> string -> t -> unit
  [@@alert blocking "Use [Writer.save_sexp ~hum:true] to avoid blocking."]

  val save_mach : ?perm:int -> string -> t -> unit
  [@@alert blocking "Use [Writer.save_sexp ~hum:false] to avoid blocking."]

  val save_sexps : ?perm:int -> string -> t list -> unit
  [@@alert blocking "Use [Writer.save_sexps ~hum:false] to avoid blocking."]

  val save_sexps_hum : ?perm:int -> string -> t list -> unit
  [@@alert blocking "Use [Writer.save_sexps ~hum:true] to avoid blocking."]

  val save_sexps_mach : ?perm:int -> string -> t list -> unit
  [@@alert blocking "Use [Writer.save_sexps ~hum:false] to avoid blocking."]
end =
    Sexp
end

let exit = Shutdown.exit

(**/**)

module Async_unix_private = struct
  module Fd_by_descr = Fd_by_descr
  module Raw_fd = Raw_fd
  module Raw_scheduler = Raw_scheduler
  module Syscall = Syscall
end

(**/**)
