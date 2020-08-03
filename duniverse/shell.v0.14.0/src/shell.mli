(** Shell scripting in OCaml.

    This module contains basic blocks for shell scripting in OCaml. It tends to
    be safer than just using [Unix.system] because it handles errors more
    strictly.
*)
open! Core

(** {6 Process handling }  *)

(**
   This type is an umbrella type for all the command that dispatch a process.
   It comes with a list of arguments whose default value can be tweaked by
   set_defaults.

   - [use_extra_path] : if we fail to find the command in the path then we look for it
     [extra_path]
   - [timeout]        : the command will raise [Failed] if the program doesn't
     do any IO for this period of time
   - [working_dir]    : run the command in this directory
   - [verbose]        : prints the output of the command
   - [echo]           : print out the command before running it
   - [input]          : a string to pipe through the program's standard in
   - [env]            : controls the environment variables passed to the command
   - [preserve_euid]  : pass the '-p' option to bash when running the command; this should
     disable the default bash behavior of replacing the effective user
     ID with the current value of the real user ID, useful in programs
     where privileges are escalated and de-escalated using seteuid(2)


   WARNING: the input argument to this function should not be used because
   it can deadlock if the input is too big (~160kb?)
*)
type 'a with_process_flags =
  ?use_extra_path:bool
  -> ?timeout:Time.Span.t option
  -> ?working_dir:string (* rename to run_in? *)
  -> ?setuid:int
  -> ?setgid:int
  -> ?env:[`Extend of (string * string) list
          |`Replace of (string * string) list]
  -> ?verbose:bool
  -> ?echo:bool
  -> ?input:string
  -> ?keep_open:bool
  -> ?tail_len:int
  -> 'a

(**
   This is the list of flags for normal process dispatch. It is an extension of
   [with_process_flags].

   - [expect] : an int list of valid return codes. default value is [[0]], if
     the return code of the dispatched is not in this list we will blowup with
     [Process.Failure]
*)
type 'a with_run_flags = ?expect:int list -> 'a with_process_flags

(** {9 Basic run functions}

    In all the functions below the command is specified with two arguments.  The
    first one is a string representing the process to run.  The second one is
    the list of arguments to pass.

    Although the arguments do not need to be escaped there is still a risk that
    they might be interpreted as flags when they aren't. Most basic unix
    utilities provide the ability to pass arguments after "--" to avoid this.

    Usage example:
    {[
      let patch = run_full ~expect:[0;1] "diff" ["-u";"--";file1;file2]
    ]}
*)

type 'a cmd = string -> string list -> 'a

(** Runs a command and discards its output. *)
val run       : unit cmd with_run_flags

(** Runs a command and returns its output line separated. Note: most commands
    print a newline at the end of their output so the shell prompt appears on
    its own line. If the output ends in a newline, it is stripped before
    splitting the output into a string list to avoid there being a final
    element in the list containing just the empty string.

    In some cases, the newline should not be stripped (e.g., "cat" will not
    "add" a newline). If you care, use [run_full] for the entire buffer.
*)
val run_lines : ?eol:char -> string list cmd with_run_flags

(** Returns the first line of the command's output.

    This function might terminate the program early the same way that
    piping through [head -n 1] would. When that happens, exit code of the
    program gets ignored!
*)
val run_first_line     : ?eol:char -> string option cmd with_run_flags
val run_first_line_exn : ?eol:char -> string cmd with_run_flags

(** Returns the only line of the command's output.
    If the command prints zero or multiple lines this returns an [Error].

    If the command exits with non-zero exit code it raises an exception. *)
val run_one_line     : ?eol:char -> string Or_error.t cmd with_run_flags
val run_one_line_exn : ?eol:char -> string cmd with_run_flags
val run_one : ?eol:char -> string option cmd with_run_flags
[@@deprecated "[since 2017-11] Use [run_one_line] to get a different behavior or \
               [run_first_line] to get the old behavior"]
val run_one_exn : ?eol:char -> string cmd with_run_flags
[@@deprecated "[since 2017-11] Use [run_one_line_exn] to get a different behavior or \
               [run_first_line_exn] to get the old behavior"]



(** Return the full command's output in one string. See the note in
    [run_lines].
*)
val run_full  : string cmd with_run_flags

(** Fold over the lines in the stdout of a process;
    The `Continue/`Stop argument is there to allow early returning.
    [eol] specifies the end of line character used to separate the lines
    outputted by the the program
*)
val run_fold  :
  ?eol:char
  -> init:'a
  -> f:('a -> string -> 'a * [ `Continue | `Stop ])
  -> 'a cmd with_run_flags


(** {9 Dispatch to /bin/bash}

    All these function take a format (like printf) and run it through the shell.

    Usage example:
    {[
      sh "cp -- %s %s" (Filename.quote file1)  (Filename.quote file2)
    ]}

    In general it is recommended to avoid using those too much and to prefer the
    run* family of function instead because it avoids pitfall like escaping
    issues and is much more straightforward to think about.
*)


type ('a,'ret) sh_cmd = ('a, unit, string,'ret) format4 -> 'a

val sh       : ('a,unit)          sh_cmd with_run_flags
val sh_lines : ('a,string list)   sh_cmd with_run_flags
val sh_full  : ('a,string)        sh_cmd with_run_flags
val sh_one   : ('a,string option) sh_cmd with_run_flags
[@@deprecated "[since 2017-11] Use [sh_one_line] to get a different behavior or \
               [sh_first_line] to get the old behavior"]
val sh_one_exn : ('a,string) sh_cmd with_run_flags
[@@deprecated "[since 2017-11] Use [sh_one_line_exn] to get a different behavior or \
               [sh_first_line_exn] to get the old behavior"]
val sh_one_line       : ('a,string Or_error.t) sh_cmd with_run_flags
val sh_one_line_exn   : ('a,string)            sh_cmd with_run_flags
val sh_first_line     : ('a,string option)     sh_cmd with_run_flags
val sh_first_line_exn : ('a,string)            sh_cmd with_run_flags

(* Magic invocation to avoid asking for password if we can.  These arguments are
   passed to ssh in the [ssh_*] functions below.  They're exposed in case you
   want to use them in a different context. *)
val noninteractive_ssh_options : string list
val noninteractive_no_hostkey_checking_options : string list

type 'a with_ssh_flags = ?ssh_options:string list -> ?user:string -> host:string -> 'a

val ssh       : ('a,unit)          sh_cmd with_run_flags with_ssh_flags
val ssh_lines : ('a,string list)   sh_cmd with_run_flags with_ssh_flags
val ssh_full  : ('a,string)        sh_cmd with_run_flags with_ssh_flags
val ssh_one   : ('a,string option) sh_cmd with_run_flags with_ssh_flags
[@@deprecated "[since 2017-11] Use [ssh_one_line] to get a different behavior or \
               [ssh_first_line] to get the old behavior"]
val ssh_one_exn : ('a,string) sh_cmd with_run_flags with_ssh_flags
[@@deprecated "[since 2017-11] Use [ssh_one_line_exn] to get a different behavior or \
               [ssh_first_line_exn] to get the old behavior"]
val ssh_one_line       : ('a,string Or_error.t) sh_cmd with_run_flags with_ssh_flags
val ssh_one_line_exn   : ('a,string)            sh_cmd with_run_flags with_ssh_flags
val ssh_first_line     : ('a,string option)     sh_cmd with_run_flags with_ssh_flags
val ssh_first_line_exn : ('a,string)            sh_cmd with_run_flags with_ssh_flags

(** {9 Test dispatches}

    Usage example:
    {[
      if Shell.test "diff" ["-q";"--";file1;file2] then
        Printf.printf "Files %S and %S are the same\n%!" file1 file2;
    ]}

*)

(** This is the list of flags for dispatching processes in test mode. This is
    used to test the return code of the dispatched program. The return value of
    these functions will be :
    - [true] if the exit code is in [true_v].
    - [false] if the exit code is in [false_v] and not in [true_v].
    - Raises [Process.Failure] otherwise

    The default values are:
    - [true_v]: default value [[0]]
    - [false_v]: default_value [[1]]
*)
type 'a with_test_flags = ?true_v:int list -> ?false_v:int list
  -> ('a with_process_flags)

val test    : bool cmd with_test_flags

val sh_test : ('a,bool) sh_cmd with_test_flags

val ssh_test : ('a,bool) sh_cmd with_test_flags with_ssh_flags

(** variable used by dispatch command to find binaries not in the path.
    The default values contains only directory which should be in PATH and is
    only useful in environments where the PATH variable has been blown away.
*)
val extra_path : string list ref

(** Process dispatching *)
module Process : sig

  type status =  [ `Timeout of Time.Span.t
                 | `Exited of int
                 | `Signaled of Signal.t
                   (* WStopped is impossible*)
                 ]
  (** The termination status of a process.
      This is an extension of [Unix.Process_status.t] to allow timeouts.
  *)

  type t

  type result = {
    command : t;
    status  : status;
    stdout  : string;
    stderr  : string
  }

  exception Failed of result

  val to_string        : t -> string
  val status_to_string : status -> string

  val set_defaults
    :  ?timeout:Time.Span.t option
    -> ?verbose:bool
    -> ?echo:bool
    -> ?preserve_euid:bool
    -> unit
    -> unit

  val format_failed : result -> string

  val cmd    : string -> string list -> t
  val shell  : string -> t

  val make_ssh_command :
    ?ssh_options:string list
    -> ?quote_args:bool
    -> ?user:string
    -> host:string
    -> string list
    -> t

  val remote :
    ?ssh_options:string list
    -> ?quote_args:bool
    -> ?user:string
    -> host:string
    -> t
    -> t

  type 'a reader

  val content : string reader
  val content_and_stderr : (string * string) reader
  val discard : unit reader

  val lines   : ?eol:char -> unit -> string list reader
  val head     : ?eol:char -> unit -> string option reader

  exception Empty_head

  val head_exn : ?eol:char -> unit -> string reader

  val one_line     : ?eol:char -> unit -> string Or_error.t reader
  val one_line_exn     : ?eol:char -> unit -> string reader

  val callback : add:(Bytes.t -> int -> unit) -> flush:(unit -> unit) -> unit reader
  val callback_with_stderr
    : add:(Bytes.t -> int -> unit)
    -> add_err:(Bytes.t -> int -> unit)
    -> flush:(unit -> unit) -> unit reader

  val run   :  (t -> 'a reader -> 'a)              with_run_flags
  val run_k : ((t -> 'a reader -> 'a) -> 'b) -> 'b with_run_flags

  val test   :  (t -> bool)              with_test_flags
  val test_k : ((t -> bool) -> 'a) -> 'a with_test_flags
end

(** {6 Small helper commands} *)

val mkdir : ?p:unit -> ?perm:int -> string -> unit

val cp :
  ?overwrite:bool
  -> ?perm:Unix.file_perm
  -> string
  -> string
  -> unit

val ln : ?s:unit -> ?f:unit -> string -> string -> unit

val rm : ?r:unit -> ?f:unit -> string -> unit

val mv : string -> string -> unit
(** Raises "Failed_command" *)

(** Get the username. By default, the effective username. If real is true, get
    the real username. *)
val whoami : ?real:bool -> unit -> string

val which : ?use_extra_path:bool -> string -> string option

(** [scp user host from to] copy local file from to to *)
val scp : ?compress:bool -> ?recurse:bool -> ?user:string -> host:string -> string -> string -> unit
