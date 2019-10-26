(** An extension of [Expect_test_helpers_kernel] with Async-aware functions. *)

open! Core
open! Async
open! Import

include module type of struct
  include Expect_test_helpers_kernel
end

(** [with_temp_dir f] creates a temporary directory which is fed to [f].  The directory
    is removed after [f] exits. *)
val with_temp_dir : (string -> 'a Deferred.t) -> 'a Deferred.t

(** [within_temp_dir ?links f] creates a temporary directory, $T, and:

    1. Adds $T/bin to the PATH environment variable.
    2. For each [file, `In_path_as, name] in [links], links [file] as $T/bin/[name].
    3. For each [file, `In_temp_as, name] in [links], links [file] as $T/[name].

    It then [cd]s to $T and calls [f].  After [f] exits, it [cd]s back, removes the
    temporary directory, and restores the original PATH.

    [within_temp_dir] creates hard links to ensure that files remain available and
    unchanged even if jenga starts to rebuild while the test is running.  If [file] and $T
    are not on the same file system, [within_temp_dir] copies the files instead of
    creating hard links. *)
val within_temp_dir
  :  ?links:(string * [`In_path_as | `In_temp_as] * string) list
  -> (unit -> 'a Deferred.t)
  -> 'a Deferred.t

(** [run prog args] creates a child process that runs [prog], with arguments [args], its
    environment extended with [extend_env] and its stdin coming from [stdin].  No
    expansion or escaping is done to [args] or [stdin].  The child process's stdout and
    stderr are captured separately for comparison. *)
val run
  :  ?enable_ocaml_backtraces:bool (** default is [false] *)
  -> ?extend_env:(string * string) list (** default is [] *)
  -> ?hide_positions:bool (** default is [false] *)
  -> ?postprocess:(string -> string) (** default is [Fn.id] *)
  -> ?print_cmdline:bool (** default is [false] *)
  -> ?print_stdout:bool (** default is [true] *)
  -> ?print_stderr:bool (** default is [true] *)
  -> ?stdin:string
  -> ?working_dir:string
  -> string
  -> string list
  -> unit Deferred.t

(** [system ?stdin cmd] creates a child process that runs [/bin/sh], with arguments
    ["-c"; cmd] and its stdin coming from [stdin]. The child process's stdout and stderr
    are captured separately for comparison. Unlike with [Unix.system], the child shell's
    stdin is never a tty, even if the stdin of this process is a tty, and the child
    shell's stderr is never copied to this process's stderr. *)
val system
  :  ?enable_ocaml_backtraces:bool (** default is [true] *)
  -> ?hide_positions:bool (** default is [false] *)
  -> ?print_cmdline:bool (** default is [false] *)
  -> ?stdin:string
  -> string
  -> unit Deferred.t

(** [show_raise' ?hide_positions ?rest f] calls [f ()] and prints either the exception
    raised by [f] or "did not raise".  [show_raise'] ignores the result of [f] so that one
    doesn't have to put an [ignore] inside [f].  [~hide_positions] operates as in
    [print_s], to make output less fragile.  Once a result is returned, the rest of the
    errors are printed to stdout. *)
val show_raise'
  :  ?hide_positions:bool (** default is [false] *)
  -> (unit -> _ Deferred.t)
  -> unit Deferred.t

(** [require_does_not_raise'] is like [require_does_not_raise], but for functions that
    produce a deferred result. *)
val require_does_not_raise'
  :  ?cr:CR.t (** default is [CR] *)
  -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
  -> ?show_backtrace:bool (** default is [false] *)
  -> Source_code_position.t
  -> (unit -> unit Deferred.t)
  -> unit Deferred.t

(** [require_does_raise'] is like [require_does_raise], but for functions that produce a
    deferred result. *)
val require_does_raise'
  :  ?cr:CR.t (** default is [CR] *)
  -> ?hide_positions:bool (** default is [false] when [cr=CR], [true] otherwise *)
  -> ?show_backtrace:bool (** default is [false] *)
  -> Source_code_position.t
  -> (unit -> _ Deferred.t)
  -> unit Deferred.t

(** We export [Expect_test_config] so that the [%expect] syntax uses Async, to prevent a
    confusing situation in which one is using [Expect_test_helpers] functions, which
    expect Async to be running, but Async isn't running. *)
module Expect_test_config = Async.Expect_test_config
