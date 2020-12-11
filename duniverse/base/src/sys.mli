(** Cross-platform system configuration values. *)

(** The command line arguments given to the process.
    The first element is the command name used to invoke the program.
    The following elements are the command-line arguments given to the program.

    When running in JavaScript in the browser, it is [[| "a.out" |]].

    [get_argv] is a function because the external function [caml_sys_modify_argv] can
    replace the array starting in OCaml 4.09. *)
val get_argv : unit -> string array

(** A single result from [get_argv ()]. This value is indefinitely deprecated. It is kept
    for compatibility with {!Caml.Sys}. *)
val argv : string array
[@@deprecated
  "[since 2019-08] Use [Sys.get_argv] instead, which has the correct behavior when \
   [caml_sys_modify_argv] is called."]

(** [interactive] is set to [true] when being executed in the [ocaml] REPL, and [false]
    otherwise. *)
val interactive : bool ref

(** [os_type] describes the operating system that the OCaml program is running on.  Its
    value is one of ["Unix"], ["Win32"], or ["Cygwin"]. When running in JavaScript, it is
    ["Unix"]. *)
val os_type : string

(** [unix] is [true] if [os_type = "Unix"]. *)
val unix : bool

(** [win32] is [true] if [os_type = "Win32"]. *)
val win32 : bool

(** [cygwin] is [true] if [os_type = "Cygwin"]. *)
val cygwin : bool

(** Currently, the official distribution only supports [Native] and [Bytecode],
    but it can be other backends with alternative compilers, for example,
    JavaScript. *)
type backend_type = Sys0.backend_type =
  | Native
  | Bytecode
  | Other of string

(** Backend type currently executing the OCaml program. *)
val backend_type : backend_type

(** [word_size_in_bits] is the number of bits in one word on the machine currently
    executing the OCaml program.  Generally speaking it will be either [32] or [64].  When
    running in JavaScript, it will be [32]. *)
val word_size_in_bits : int

(** [int_size_in_bits] is the number of bits in the [int] type.  Generally, on
    32-bit platforms, its value will be [31], and on 64 bit platforms its value
    will be [63]. When running in JavaScript, it will be [32]. {!Int.num_bits}
    is the same as this value. *)
val int_size_in_bits : int

(** [big_endian] is true when the program is running on a big-endian
    architecture.  When running in JavaScript, it will be [false]. *)
val big_endian : bool

(** [max_string_length] is the maximum allowed length of a [string] or [Bytes.t].
    {!String.max_length} is the same as this value. *)
val max_string_length : int

(** [max_array_length] is the maximum allowed length of an ['a array].
    {!Array.max_length} is the same as this value. *)
val max_array_length : int

(** Returns the name of the runtime variant the program is running on.  This is normally
    the argument given to [-runtime-variant] at compile time, but for byte-code it can be
    changed after compilation.

    When running in JavaScript or utop it will be [""], while if compiled with DEBUG
    (debugging of the runtime) it will be ["d"], and if compiled with CAML_INSTR
    (instrumentation of the runtime) it will be ["i"]. *)
val runtime_variant : unit -> string

(** Returns the value of the runtime parameters, in the same format as the contents of the
    [OCAMLRUNPARAM] environment variable.  When running in JavaScript, it will be [""]. *)
val runtime_parameters : unit -> string

(** [ocaml_version] is the OCaml version with which the program was compiled.  It is a
    string of the form ["major.minor[.patchlevel][+additional-info]"], where major, minor,
    and patchlevel are integers, and additional-info is an arbitrary string.  The
    [[.patchlevel]] and [[+additional-info]] parts may be absent. *)
val ocaml_version : string

(** Controls whether the OCaml runtime system can emit warnings on stderr. Currently, the
    only supported warning is triggered when a channel created by [open_*] functions is
    finalized without being closed. Runtime warnings are enabled by default. *)
val enable_runtime_warnings : bool -> unit

(** Returns whether runtime warnings are currently enabled. *)
val runtime_warnings_enabled : unit -> bool

(** Return the value associated to a variable in the process environment. Return [None] if
    the variable is unbound or the process has special privileges, as determined by
    [secure_getenv(3)] on Linux. *)
val getenv : string -> string option

val getenv_exn : string -> string

(** For the purposes of optimization, [opaque_identity] behaves like an unknown (and thus
    possibly side-effecting) function.  At runtime, [opaque_identity] disappears
    altogether.  A typical use of this function is to prevent pure computations from being
    optimized away in benchmarking loops.  For example:

    {[
      for _round = 1 to 100_000 do
        ignore (Sys.opaque_identity (my_pure_computation ()))
      done
    ]} *)
external opaque_identity : 'a -> 'a = "%opaque"
