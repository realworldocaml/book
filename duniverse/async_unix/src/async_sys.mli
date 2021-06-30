(** This module overrides everything in the [Core.Sys] module that might block.  Functions
    do the same thing as their counterparts in [Core.Sys], but instead return deferreds.
    For a description of their semantics see the documentation for the [Core.Sys]
    module. *)

open! Core
open! Import

val argv : string array
[@@deprecated
  "[since 2019-08] Use [Sys.get_argv] instead, which has the correct behavior when \
   [caml_sys_modify_argv] is called."]

val get_argv : unit -> string array
val executable_name : string
val file_exists : ?follow_symlinks:bool -> string -> [ `Yes | `No | `Unknown ] Deferred.t
val file_exists_exn : ?follow_symlinks:bool -> string -> bool Deferred.t

(** [when_file_exists ?poll_delay file] returns a deferred that becomes determined when
    [file] exists.  The default poll delay is 0.5 seconds.  It raises an exception if it
    can not check whether the file is there, in the same cases [file_exists] returns
    [`Unknown]. *)
val when_file_exists
  :  ?follow_symlinks:bool
  -> ?poll_delay:Time.Span.t
  -> string
  -> unit Deferred.t


(** [when_file_changes file] polls [file] using [stat] and writes [file]'s mtime to the
    pipe every time it changes.  The first time in the pipe will be [file]'s current
    mtime.  To stop polling, close the pipe. *)
val when_file_changes
  :  ?on_exn:(exn -> unit)
  -> ?poll_delay:Time.Span.t
  -> string
  -> Time.t Pipe.Reader.t

val is_directory
  :  ?follow_symlinks:bool
  -> string
  -> [ `Yes | `No | `Unknown ] Deferred.t

val is_directory_exn : ?follow_symlinks:bool -> string -> bool Deferred.t
val is_file : ?follow_symlinks:bool -> string -> [ `Yes | `No | `Unknown ] Deferred.t
val is_file_exn : ?follow_symlinks:bool -> string -> bool Deferred.t
val remove : string -> unit Deferred.t
val rename : string -> string -> unit Deferred.t
val getenv : string -> string option
val getenv_exn : string -> string
val command : string -> int Deferred.t
val command_exn : string -> unit Deferred.t
val quote : string -> string
val chdir : string -> unit Deferred.t
val getcwd : unit -> string Deferred.t
val readdir : string -> string array Deferred.t
val ls_dir : string -> string list Deferred.t
val home_directory : unit -> string Deferred.t
val opaque_identity : 'a -> 'a
val interactive : bool ref
val os_type : string
val word_size : int
val int_size : int
val big_endian : bool
val ocaml_version : string
val execution_mode : unit -> [ `Bytecode | `Native ]
val c_int_size : unit -> int
