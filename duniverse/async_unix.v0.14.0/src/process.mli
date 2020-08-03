(** [Async.Process] is for creating child processes of the current process, and
    communicating with children via their stdin, stdout, and stderr.  [Async.Process] is
    the Async analog of [Core.Unix.create_process] and related functions. *)

open! Core
open! Import

type t [@@deriving sexp_of]

(** accessors *)

val pid : t -> Pid.t
val stdin : t -> Writer.t
val stdout : t -> Reader.t
val stderr : t -> Reader.t

type env = Unix.env [@@deriving sexp]

(** [create ~prog ~args ()] uses [Core.Unix.create_process_env] to create a child process
    that runs the executable [prog] with [args] as arguments.  It creates pipes to
    communicate with the child process's [stdin], [stdout], and [stderr].

    Unlike [exec], [args] should not include [prog] as the first argument.

    If [buf_len] is supplied, it determines the size of the reader and writer buffers used
    to communicate with the child process's [stdin], [stdout], and [stderr].

    If [stdin] is supplied, then the writer to the child's stdin will have
    [~raise_when_consumer_leaves:false] and [~buffer_age_limit:`Unlimited], which makes it
    more robust.

    [env] specifies the environment of the child process.

    If [working_dir] is supplied, then the child process will [chdir()] there before
    calling [exec()].

    If [argv0] is given, it is used (instead of [prog]) as the first element of the [argv]
    array passed to [exec].

    [create] returns [Error] if it is unable to create the child process.  This can happen
    in any number of situations (unable to fork, unable to create the pipes, unable to cd
    to [working_dir], unable to [exec] etc.).  [create] does not return [Error] if the
    binary exits with non-zero exit code; instead, it returns [OK t], where [wait t]
    returns an [Error].

    See [Core.Unix.create_process_env] for more details. *)
type 'a create =
  ?argv0:string
  -> ?buf_len:int
  -> ?env:env (** default is [`Extend []] *)
  -> ?prog_search_path:string list
  -> ?stdin:string
  -> ?working_dir:string
  -> prog:string
  -> args:string list
  -> unit
  -> 'a Deferred.t

val create : t Or_error.t create
val create_exn : t create

(** [wait t = Unix.waitpid (pid t)].  [wait]'s result becomes determined when the child
    process terminates, via exit or signal.  [wait] does not touch [stdin], [stdout] or
    [stderr].  The caller should ensure that [stdout] and [stderr] are being drained in
    the background to avoid the child process blocking on a write due to pushback.  See
    [collect_output_and_wait] for a higher-level alternative that handles this. *)
val wait : t -> Unix.Exit_or_signal.t Deferred.t

module Output : sig
  type t =
    { stdout : string
    ; stderr : string
    ; exit_status : Unix.Exit_or_signal.t
    }
  [@@deriving compare, sexp_of]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving compare, sexp]
    end
  end
end

(** [collect_output_and_wait t] closes [stdin t] and then begins collecting the output
    produced on [t]'s [stdout] and [stderr], continuing to collect output until [t]
    terminates and the pipes for [stdout] and [stderr] are closed.  Usually when [t]
    terminates, the pipes are closed; however, [t] could fork other processes which
    survive after [t] terminates and in turn keep the pipes open --
    [collect_output_and_wait] will not become determined until both pipes are closed in
    all descendant processes. *)
val collect_output_and_wait : t -> Output.t Deferred.t

(** [run] [create]s a process, feeds it [stdin] if provided, and [wait]s for it to
    complete.  If the process exits with an acceptable status, then [run] returns its
    stdout.  If the process exits unacceptably, then [run] returns an error indicating
    what went wrong that includes stdout and stderr.

    Acceptable statuses are zero, and any nonzero values specified in
    [accept_nonzero_exit].

    Some care is taken so that an error displays nicely as a sexp---in particular, if the
    child's output can already be parsed as a sexp, then it will display as a sexp (rather
    than a sexp embedded in a string).  Also, if the output isn't a sexp, it will be split
    on newlines into a list of strings, so that it displays on multiple lines rather than
    a single giant line with embedded "\n"'s.

    [run_lines] is like [run] but returns the lines of stdout as a string list, using
    [String.split_lines].

    [run_expect_no_output] is like [run] but expects the command to produce no output, and
    returns an error if the command does produce output. *)
type 'a run =
  ?accept_nonzero_exit:int list (** default is [] *)
  -> ?argv0:string
  -> ?env:env (** default is [`Extend []] *)
  -> ?prog_search_path:string list
  -> ?stdin:string
  -> ?working_dir:string
  -> prog:string
  -> args:string list
  -> unit
  -> 'a Deferred.t

val run : string Or_error.t run
val run_exn : string run
val run_lines : string list Or_error.t run
val run_lines_exn : string list run
val run_expect_no_output : unit Or_error.t run
val run_expect_no_output_exn : unit run

(** [collect_stdout_and_wait] and [collect_stdout_lines_and_wait] are like [run] and
    [run_lines] but work from an existing process instead of creating a new one. *)
type 'a collect =
  ?accept_nonzero_exit:int list (** default is [] *) -> t -> 'a Deferred.t

val collect_stdout_and_wait : string Or_error.t collect
val collect_stdout_and_wait_exn : string collect
val collect_stdout_lines_and_wait : string list Or_error.t collect
val collect_stdout_lines_and_wait_exn : string list collect

(** Sends a signal to this process. This is safe to call concurrently with [wait t], even
    if the Pid is reused after the process died.

    If the process was already terminated, the call succeeds and silently does nothing,
    regardless of whether or not the process was waited for. *)
val send_signal : t -> Signal.t -> unit

(** [Lines_or_sexp] is useful for rendering a string nicely in a sexp, avoiding quoting if
    the string is multi-line or was produced by converting a sexp to a string.
    [Output.sexp_of_t] uses [Lines_or_sexp] to nicely render stdout and stderr of a child
    process. *)
module Lines_or_sexp : sig

  type t [@@deriving sexp_of]

  val create : string -> t
end
