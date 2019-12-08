(**
   Low-level process handling

   This is low-level enough that you should probably be using [Shell] instead
   to dispatch processes.
*)

open! Core

module Status : sig
  type t =   [ `Timeout of Time.Span.t
             | `Exited of int
             | `Signaled of Signal.t
               (* WStopped is impossible*)
             ]
  [@@deriving sexp_of]

  val to_string : t -> string

end

module Command_result : sig
  type t= {
    status      : Status.t;
    stdout_tail : string;
    stderr_tail : string
  }
end

(** kills a process by sending [signal]; waiting for [wait_for] and then
    sending a [sigkill].
    You need to set is_child to true when killing child processes or run waitpid
    on them in another.
    @raises Failure if the target program hangs for more that [wait_for] after
    receiving the [sigkill].

    caveat: [is_child:false] (the default) is racy: it can both send signals to wrong
    processes and it can also fail to notice that the target died.
*)
val kill :
  ?is_child:bool ->
  ?wait_for:Time.Span.t ->
  ?signal:Signal.t ->
  Pid.t
  -> unit

(**
   Runs the process.

   [stdoutf s len] and [stderrf s len] should only inspect the [String.subo s ~len]
   component of [s]. *)
val run :
  ?timeout:Time.Span.t
  -> ?use_extra_path:bool
  -> ?working_dir:string
  -> ?setuid:int
  -> ?setgid:int
  -> ?env:([`Extend of (string * string) list
           | `Replace of (string * string) list])
  -> ?input:string
  -> ?keep_open:bool
  -> ?stdoutf:(Bytes.t -> int -> unit)
  -> ?stderrf:(Bytes.t -> int -> unit)
  -> ?tail_len:int
  -> prog:string
  -> args:string list
  -> unit
  -> Command_result.t
