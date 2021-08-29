(** Module for managing stack backtraces.

    The [Backtrace] module deals with two different kinds of backtraces:

    + Snapshots of the stack obtained on demand ([Backtrace.get])
    + The stack frames unwound when an exception is raised ([Backtrace.Exn])
*)

open! Import

(** A [Backtrace.t] is a snapshot of the stack obtained by calling [Backtrace.get].  It is
    represented as a string with newlines separating the frames.  [sexp_of_t] splits the
    string at newlines and removes some of the cruft, leaving a human-friendly list of
    frames, but [to_string] does not. *)
type t [@@deriving_inline sexp_of]

val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

[@@@end]

val get : ?at_most_num_frames:int -> unit -> t
val to_string : t -> string
val to_string_list : t -> string list

(** The value of [elide] controls the behavior of backtrace serialization functions such
    as {!to_string}, {!to_string_list}, and {!sexp_of_t}.  When set to [false], these
    functions behave as expected, returning a faithful representation of their argument.
    When set to [true], these functions will ignore their argument and return a message
    indicating that behavior.

    The default value is {!am_testing}. *)
val elide : bool ref

(** [Backtrace.Exn] has functions for controlling and printing the backtrace of the most
    recently raised exception.

    When an exception is raised, the runtime "unwinds" the stack, i.e., removes stack
    frames, until it reaches a frame with an exception handler. It then matches the
    exception against the patterns in the handler. If the exception matches, then the
    program continues. If not, then the runtime continues unwinding the stack to the next
    handler.

    If [am_recording () = true], then while the runtime is unwinding the stack, it keeps
    track of the part of the stack that is unwound. This is available as a backtrace via
    [most_recent ()]. Calling [most_recent] if [am_recording () = false] will yield the
    empty backtrace.

    With [am_recording () = true], OCaml keeps only a backtrace for the most recently
    raised exception. When one raises an exception, OCaml checks if it is physically equal
    to the most recently raised exception. If it is, then OCaml appends the string
    representation of the stack unwound by the current raise to the stored backtrace. If
    the exception being raised is not physically equally to the most recently raised
    exception, then OCaml starts recording a new backtrace. Thus one must call
    [most_recent] before a subsequent [raise] of a (physically) distinct exception, or the
    backtrace is lost.

    The initial value of [am_recording ()] is determined by the environment variable
    OCAMLRUNPARAM. If OCAMLRUNPARAM is set and contains a "b" parameter, then
    [am_recording ()] is set according to OCAMLRUNPARAM: true if "b" or "b=1" appears;
    false if "b=0" appears. If OCAMLRUNPARAM is not set (as is always the case when
    running in a web browser) or does not contain a "b" parameter, then [am_recording ()]
    is initially true.

    This is the same functionality as provided by the OCaml stdlib [Printexc] functions
    [backtrace_status], [record_backtraces], [get_backtrace]. *)
module Exn : sig
  val am_recording : unit -> bool
  val set_recording : bool -> unit
  val with_recording : bool -> f:(unit -> 'a) -> 'a

  (** [most_recent ()] returns a backtrace containing the stack that was unwound by the
      most recently raised exception. *)
  val most_recent : unit -> t
end

(** User code never calls this.  It is called only in [std_kernel.ml], as a top-level side
    effect, to initialize [am_recording ()] as specified above. *)
val initialize_module : unit -> unit
