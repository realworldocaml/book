(** [Lock_file_async] is a wrapper that provides Async equivalents for
    {{!Lock_file_blocking}[Lock_file_blocking]}. *)

open! Core
open! Async
open! Import

(** [create ?message path] tries to create a file at [path] containing the text [message],
    pid if none provided.  It returns true on success, false on failure.  Note: there is
    no way to release the lock or the fd created inside!  It will only be released when
    the process dies.*)
val create
  :  ?message        : string
  -> ?close_on_exec  : bool    (** default is [true] *)
  -> ?unlink_on_exit : bool    (** default is [false] *)
  -> string
  -> bool Deferred.t

(** [create_exn ?message path] is like [create] except that it throws an exception on
    failure instead of returning a boolean value. *)
val create_exn
  :  ?message        : string
  -> ?close_on_exec  : bool    (** default is [true] *)
  -> ?unlink_on_exit : bool    (** default is [false] *)
  -> string
  -> unit Deferred.t

(** [waiting_create path] repeatedly tries to lock [path], becoming determined when [path]
    is locked or raising when [abort] becomes determined.  Similar to
    {!Lock_file_blocking.create}. *)
val waiting_create
  :  ?abort          : unit Deferred.t  (** default is [Deferred.never ()] *)
  -> ?message        : string
  -> ?close_on_exec  : bool             (** default is [true] *)
  -> ?unlink_on_exit : bool             (** default is [false] *)
  -> string
  -> unit Deferred.t

(** [is_locked path] returns true when the file at [path] exists and is locked, false
    otherwise. *)
val is_locked : string -> bool Deferred.t

(** [Nfs] has analogs of functions in {!Lock_file_blocking.Nfs}; see
    there for documentation.  In addition to adding [Deferred]'s, [blocking_create] was
    renamed [waiting_create] to avoid the impression that it blocks Async. *)
module Nfs : sig
  val create     : ?message : string -> string -> unit Deferred.Or_error.t
  val create_exn : ?message : string -> string -> unit Deferred.t

  val waiting_create
    :  ?abort:unit Deferred.t  (** default is [Deferred.never ()]. *)
    -> ?message : string
    -> string
    -> unit Deferred.t

  val unlock_exn     : string -> unit Deferred.t
  val unlock         : string -> unit Deferred.Or_error.t

  val critical_section
    :  ?message : string
    -> string
    -> abort:unit Deferred.t
    -> f : (unit -> 'a Deferred.t)
    -> 'a Deferred.t

  val get_hostname_and_pid : string -> (string * Pid.t) option Deferred.t
  val get_message          : string ->  string          option Deferred.t
end

module Flock : sig
  (** [Flock] has async analogues of functions in
      {{!Lock_file_blocking.Flock}[Lock_file_blocking.Flock]}; see there for
      documentation.

      Additionally, here we:

      - catch unix exceptions, packaging them as [Deferred.Or_error.t]
      - implement abortable waiting versions based on polling *)
  type t

  val lock_exn
    :  lock_path:string
    -> [`Somebody_else_took_it | `We_took_it of t] Deferred.t

  val lock
    :  lock_path:string
    -> [`Somebody_else_took_it | `We_took_it of t] Deferred.Or_error.t

  val unlock_exn : t -> unit Deferred.t
  val unlock : t -> unit Deferred.Or_error.t

  (** [wait_for_lock_exn ?abort ~lock_path ()] Wait for the lock, giving up once [abort]
      becomes determined *)
  val wait_for_lock_exn
    :  ?abort:unit Deferred.t (** default is [Deferred.never ()] *)
    -> lock_path:string
    -> unit
    -> t Deferred.t

  (** See [wait_for_lock_exn] *)
  val wait_for_lock
    :  ?abort:unit Deferred.t
    -> lock_path:string
    -> unit
    -> t Deferred.Or_error.t
end

module Symlink : sig
  (** [Symlink] has async analogues of functions in
      {{!Lock_file_blocking.Symlink}[Lock_file_blocking.Symlink]}; see there for
      documentation.

      Additionally, here we:

      - catch unix exceptions, packaging them as [Deferred.Or_error.t]
      - implement abortable waiting versions based on polling *)
  type t

  val lock_exn
    :  lock_path:string
    -> metadata:string
    -> [`Somebody_else_took_it of string Or_error.t | `We_took_it of t] Deferred.t

  val lock
    :  lock_path:string
    -> metadata:string
    -> [`Somebody_else_took_it of string Or_error.t | `We_took_it of t] Deferred.Or_error.t

  val unlock_exn : t -> unit Deferred.t
  val unlock : t -> unit Deferred.Or_error.t

  (** [wait_for_lock_exn ?abort ~lock_path ()] Wait for the lock, giving up once [abort]
      becomes determined *)
  val wait_for_lock_exn
    :  ?abort:unit Deferred.t (** default is [Deferred.never ()] *)
    -> lock_path:string
    -> metadata:string
    -> unit
    -> t Deferred.t

  (** See [wait_for_lock_exn] *)
  val wait_for_lock
    :  ?abort:unit Deferred.t
    -> lock_path:string
    -> metadata:string
    -> unit
    -> t Deferred.Or_error.t
end
