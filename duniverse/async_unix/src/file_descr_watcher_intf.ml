(** [File_descr_watcher_intf.S] provides an API for for watching a set of file descriptors
    to see if they are ready for reading or writing.

    We have two implementations, one using epoll, and one using select.

    None of the functions need to be thread-safe, with the exception of
    [thread_safe_check].  So that implementations can easily do non-thread-safe actions,
    checking for ready I/O is always done in three steps:

    1. [pre_check], while holding the async lock
    2. [thread_safe_check], while not holding the async lock
    3. [post_check], while holding the async lock *)

open! Core
open Import

module Timeout = struct
  type 'a t =
    (*_ performance hack: avoid allocation *)
    | Never : unit t
    | Immediately : unit t
    | After : Time_ns.Span.t t

  let variant_of
    : type a. a t -> a -> [ `Never | `Immediately | `After of Time_ns.Span.t ]
    =
    fun t span_or_unit ->
    match t with
    | Never -> `Never
    | Immediately -> `Immediately
    | After -> `After (span_or_unit : Time_ns.Span.t)
  ;;
end

module type S = sig
  (** A file-descr-watcher is essentially a map from [File_descr.t] to [bool
      Read_write.t], which defines the set of file descriptors being watched, and for each
      file descriptor, whether it is being watched for read, write, or both.  If a file
      descriptor is not being watched for either, it is not in the map. *)
  type t [@@deriving sexp_of]

  include Invariant.S with type t := t

  (** [additional_create_args] abstracts over the additional arguments to different
      file-descr-watcher's [create] function. *)
  type 'a additional_create_args

  (** [create ~num_file_descrs] creates a new file-descr-watcher that is able to watch
      file descriptors in {[ [0, num_file_descrs) ]}. *)
  val create
    : (num_file_descrs:int
       -> handle_fd_read_ready:(File_descr.t -> unit)
       -> handle_fd_write_ready:(File_descr.t -> unit)
       -> t)
        additional_create_args

  val backend : Config.File_descr_watcher.t

  (** [set] alters the map of file descriptors being watched.  It will take effect on the
      next call to [thread_safe_check].  Calling [set fd] with [{ read = false, write =
      false }] removes [fd] from the map. *)
  val set : t -> File_descr.t -> bool Read_write.t -> unit

  (** [iter t ~f] iterates over every file descriptor in the map, apply [f] to it once
      for each of \{`Read,`Write\} that it is being watched for. *)
  val iter : t -> f:(File_descr.t -> Read_write.Key.t -> unit) -> unit

  (** [pre_check t] does whatever non-thread-safe work is necessary to prepare for the
      system call that checks file descriptors being ready for read or write.  [pre_check]
      does not side effect [t]. *)
  module Pre : sig
    type t [@@deriving sexp_of]
  end

  val pre_check : t -> Pre.t

  (** [thread_safe_check t pre timeout span_or_unit] checks the file descriptors for their
      status and returns when at least one is available, or the [timeout, span_or_unit]
      passes.  [thread_safe_check] does not side effect [t].  Unlike the rest of the
      functions in this module, [thread_safe_check] is thread safe. *)
  module Check_result : sig
    type t [@@deriving sexp_of]
  end

  val thread_safe_check : t -> Pre.t -> 'a Timeout.t -> 'a -> Check_result.t

  (** [post_check t check_result] calls the [handle_fd*] functions supplied to [create]:

      1. for each file descriptor that is ready to be written to, then
      2. for each file descriptor that is ready to be read from.

      We handle writes before reads so that we get all the writes started going to the
      external world before we process all the reads.  This will nicely batch together
      all the output based on the reads for the next writes.

      It is guaranteed that it calls [handle_fd_read*] only on an [fd] that is watched for
      read as per [set], and [handle_fd_write*] only on an [fd] that is watched for write
      as per [set]. *)
  val post_check : t -> Check_result.t -> unit

  val reset_in_forked_process : t -> unit
end
