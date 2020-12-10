(** An [Fd.t] is a wrapper around a Unix file descriptor, with additional information
    about the kind of file descriptor and logic to ensure that we don't use a file
    descriptor that has been closed, or close a file descriptor that is in use.

    Since Async uses multiple threads to make read/write and other system calls on file
    descriptors, and Unix reuses descriptors after they are closed, Async has to be very
    careful that the file descriptor passed to a system call is referring to the file it
    intends, and not some other completely unrelated file that Unix has decided to assign
    to the same descriptor.

    Provided that one only accesses a file descriptor within the context of the functions
    below, [Fd] guarantees that the file descriptor will not have been closed/reused and
    will correspond to the same file that it did when the [Fd.t] was created:

    {v
      with_file_descr
      with_file_descr_deferred
      syscall
      syscall_exn
      syscall_result_exn
      syscall_in_thread
      syscall_in_thread_exn
    v}

    The [Fd] module keeps track of which of these functions are currently accessing the
    file descriptor, and ensures that any close happens after they complete.  Also, once
    close has been called, it refuses to provide further access to the file descriptor,
    either by returning a variant [`Already_closed] or by raising an exception.

    Some of the above functions take an optional [?nonblocking:bool] argument.  The
    default is [false], but if it is set to [true], then before supplying the underlying
    [file_descr], the [Fd] module will first call [Unix.set_nonblock file_descr], if it
    hasn't previously done so on that file descriptor.  This is intended to support making
    nonblocking system calls (e.g., connect, read, write) directly within Async, without
    releasing the OCaml lock or the Async lock, and without using another thread. *)

open! Core
open! Import

module Kind : sig
  type t =
    | Char (** a terminal *)
    | Fifo (** a pipe *)
    | File (** a regular file *)
    | Socket of
        [ `Unconnected (** the result of [socket()] *)
        | `Bound (** the result of [bind()] *)
        | `Passive (** the result of [listen()] *)
        | `Active (** the result of [connect()] or [accept()] *)
        ]

  val infer_using_stat : Unix.File_descr.t -> t Deferred.t
end

type t [@@deriving sexp_of]
type t_hum = t [@@deriving sexp_of]

val info : t -> Info.t

(** [to_string t] returns a pretty sexp of the representation of [t]. *)
val to_string : t -> string

(** [create ?support_nonblock kind file_descr] creates a new [t] of the underlying kind
    and file descriptor.

    We thought about using [fstat()] rather than requiring the user to supply the kind.
    But [fstat] can block, which would require putting this in a thread, which has some
    consequences, and it isn't clear that it gets us that much.  Also, [create] is mostly
    used within the Async implementation -- clients shouldn't need it unless they are
    mixing Async and non-Async code.

    If [avoid_nonblock_if_possible], then Async will treat the file descriptor as blocking
    if it can (more precisely, if it's not a bound socket). *)
val create
  :  ?avoid_nonblock_if_possible:bool (** default is [false] *)
  -> Kind.t
  -> Unix.File_descr.t
  -> Info.t
  -> t

(** [kind t] returns the kind of file descriptor that [t] is. *)
val kind : t -> Kind.t

(** [supports_nonblock t] returns true if [t] supports nonblocking system calls. *)
val supports_nonblock : t -> bool

(** [clear_nonblock t] clears the [nonblocking] flag on [t] and causes Async to treat the
    fd as though it doesn't support nonblocking I/O.  This is useful for applications that
    want to share a file descriptor between Async and non-Async code and want to avoid
    [EWOULDBLOCK] or [EAGAIN] being seen by the non-Async code, which would then cause a
    [Sys_blocked_io] exception.

    [clear_nonblock t] has no effect if [not (supports_nonblock t)]. *)
val clear_nonblock : t -> unit

(** The [Close] module exists to collect [close] and its associated types, so they
    can be easily reused elsewhere, e.g., [Unix_syscalls]. *)
module Close : sig
  type socket_handling =
    | Shutdown_socket
    | Do_not_shutdown_socket

  type file_descriptor_handling =
    | Close_file_descriptor of socket_handling
    | Do_not_close_file_descriptor

  (** [close t] prevents further use of [t], and makes [shutdown()] and [close()] system
      calls on [t]'s underlying file descriptor according to the
      [file_descriptor_handling] argument and whether or not [t] is a socket, i.e., [kind
      t = Socket `Active]:

      {v
        | file_descriptor_handling                     | shutdown() | close() |
        |----------------------------------------------+------------+---------|
        | Do_not_close_file_descriptor                 | no         | no      |
        | Close_file_descriptor Shutdown_socket        | if socket  | yes     |
        | Close_file_descriptor Do_not_shutdown_socket | no         | yes     |
      v}

      The result of [close] becomes determined once the system calls complete.  It is OK
      to call [close] multiple times on the same [t]; calls subsequent to the initial call
      will have no effect, but will return the same deferred as the original call. *)
  val close
    :  ?file_descriptor_handling:file_descriptor_handling
    (** default is [Close_file_descriptor Shutdown_socket] *)
    -> t
    -> unit Deferred.t
end

include module type of Close

(** [close_started t] becomes determined when [close t] is called. *)
val close_started : t -> unit Deferred.t

(** [close_finished] returns the same result as [close], but differs in that it does not
    have the side effect of initiating a close. *)
val close_finished : t -> unit Deferred.t

(** [is_closed t] returns [true] iff [close t] has been called. *)
val is_closed : t -> bool

(** [with_close t f] applies [f] to [t], returns the result of [f], and closes [t]. *)
val with_close : t -> f:(t -> 'a Deferred.t) -> 'a Deferred.t

(** [is_open t] is [not (is_closed t]) *)
val is_open : t -> bool

(** [stdin], [stdout], and [stderr] are wrappers around the standard Unix file
    descriptors. *)
val stdin : unit -> t

val stdout : unit -> t
val stderr : unit -> t

(** [with_file_descr t f] runs [f] on the file descriptor underlying [t], if [is_open t],
    and returns [`Ok] or [`Error] according to [f].  If [is_closed t], then it does not
    call [f] and returns [`Already_closed]. *)
val with_file_descr
  :  ?nonblocking:bool (** default is [false] *)
  -> t
  -> (Unix.File_descr.t -> 'a)
  -> [ `Ok of 'a | `Already_closed | `Error of exn ]

(** [with_file_descr_exn] is like [with_file_descr] except that it raises rather than
    return [`Already_closed] or [`Error]. *)
val with_file_descr_exn
  :  ?nonblocking:bool (** default is [false] *)
  -> t
  -> (Unix.File_descr.t -> 'a)
  -> 'a

(** [with_file_descr_deferred t f] runs [f] on the file descriptor underlying [t], if
    [is_open t], and returns [`Ok] or [`Error] according to [f].  If [is_closed t], then
    it does not call [f] and returns [`Already_closed].  It ensures that the file
    descriptor underlying [t] is not closed until the result of [f] becomes determined (or
    [f] raises). *)
val with_file_descr_deferred
  :  t
  -> (Unix.File_descr.t -> 'a Deferred.t)
  -> [ `Ok of 'a | `Already_closed | `Error of exn ] Deferred.t

(** [with_file_descr_deferred_exn] is like [with_file_descr_deferred], except that it
    raises rather than return [`Already_closed] or [`Error]. *)
val with_file_descr_deferred_exn
  :  t
  -> (Unix.File_descr.t -> 'a Deferred.t)
  -> 'a Deferred.t

(** [interruptible_ready_to t read_write ~interrupt] returns a deferred that will become
    determined when the file descriptor underlying [t] can be read from or written to
    without blocking, or when [interrupt] becomes determined. *)
val interruptible_ready_to
  :  t
  -> [ `Read | `Write ]
  -> interrupt:unit Deferred.t
  -> [ `Bad_fd | `Closed | `Interrupted | `Ready ] Deferred.t

(** [ready_to t read_write] is like [interruptible_ready_to], but without the possibility
    of interruption. *)
val ready_to : t -> [ `Read | `Write ] -> [ `Bad_fd | `Closed | `Ready ] Deferred.t


(** [interruptible_every_ready_to t read_write ~interrupt f a] checks every Async cycle
    whether the file descriptor underlying [t] can be read from or written to without
    blocking, and if so, enqueues a job to run [f a].  [interruptible_every_ready_to] is
    level triggered -- it will enqueue a job every cycle if I/O is available, even if the
    prior job hasn't run yet, or the job ran but did not consume the available data.
    [interruptible_every_ready_to] returns a deferred that will become determined when
    [interrupt] becomes determined or the file descriptor is closed. *)
val interruptible_every_ready_to
  :  t
  -> [ `Read | `Write ]
  -> interrupt:unit Deferred.t
  -> ('a -> unit)
  -> 'a
  -> [ `Bad_fd | `Closed | `Unsupported | `Interrupted ] Deferred.t

(** [every_ready_to t read_write f x] is like [interruptible_every_ready_to], but without
    the possibility of interruption. *)
val every_ready_to
  :  t
  -> [ `Read | `Write ]
  -> ('a -> unit)
  -> 'a
  -> [ `Bad_fd | `Closed | `Unsupported ] Deferred.t

(** [syscall t f] runs [Async_unix.syscall] with [f] on the file descriptor underlying
    [t], if [is_open t], and returns [`Ok] or [`Error] according to [f].  If
    [is_closed t], it does not call [f] and returns [`Already_closed]. *)
val syscall
  :  ?nonblocking:bool (** default is [false] *)
  -> t
  -> (Unix.File_descr.t -> 'a)
  -> [ `Already_closed | `Ok of 'a | `Error of exn ]

(** [syscall_exn t f] is like [syscall], except it raises rather than return
    [`Already_closed] or [`Error]. *)
val syscall_exn
  :  ?nonblocking:bool (** default is [false] *)
  -> t
  -> (Unix.File_descr.t -> 'a)
  -> 'a

(** [syscall_result_exn t f a] is like [syscall_exn], except it does not allocate except
    in exceptional cases.  [a] is passed unchanged to [f], and should be used to eliminate
    allocations due to closure capture. *)
val syscall_result_exn
  :  ?nonblocking:bool (** default is [false] *)
  -> t
  -> 'a
  -> (Unix.File_descr.t -> 'a -> 'b Unix.Syscall_result.t)
  -> 'b Unix.Syscall_result.t

(** [syscall_in_thread t f] runs [In_thread.syscall] with [f] on the file descriptor
    underlying [t], if [is_open t], and returns a deferred that becomes determined with
    [`Ok] or [`Error] when the system call completes.  If [is_closed t], it does not call
    [f] and returns [`Already_closed]. *)
val syscall_in_thread
  :  t
  -> name:string
  -> (Unix.File_descr.t -> 'a)
  -> [ `Already_closed | `Ok of 'a | `Error of exn ] Deferred.t

(** [syscall_in_thread_exn] is like [syscall_in_thread], except it raises rather than
    return [`Already_closed] or [`Error]. *)
val syscall_in_thread_exn
  :  t
  -> name:string
  -> (Unix.File_descr.t -> 'a)
  -> 'a Deferred.t

(** [of_in_channel] and [of_out_channel] create an fd from their underlying file
    descriptor. *)
val of_in_channel : In_channel.t -> Kind.t -> t

val of_out_channel : Out_channel.t -> Kind.t -> t

(** [of_in_channel_auto ic] is just like [of_in_channel], but uses [fstat] to determine
    the kind.  It makes some assumptions about sockets, specifically it assumes that a
    socket is either listening or connected to something (and it uses [getsockopt] to find
    out which).  Don't pass an [in_channel] containing an unconnected non-listening
    socket. *)
val of_in_channel_auto : In_channel.t -> t Deferred.t

(** [of_out_channel_auto ic] is just like [of_out_channel], but uses [fstat] to determine
    the kind.  It makes some assumptions about sockets, specifically it assumes that a
    socket is either listening or connected to something (and it uses [getsockopt] to find
    out which).  Don't pass an [in_channel] containing an unconnected non listening
    socket. *)
val of_out_channel_auto : Out_channel.t -> t Deferred.t

(** [file_descr_exn t] returns the file descriptor underlying [t], unless [is_closed t],
    in which case it raises.  One must be very careful when using this function, and
    should try not to, since any uses of the resulting file descriptor are unknown to
    the [Fd] module, and hence can violate the guarantee it is trying to enforce. *)
val file_descr_exn : t -> Unix.File_descr.t

(** [to_int_exn t] returns the the underlying file descriptor as an int.  It has the same
    caveats as [file_descr_exn]. *)
val to_int_exn : t -> int

(**/**)

module Private : sig
  (** [replace t kind] is for internal use only, by [Unix_syscalls].  It is used when one
      wants to reuse a file descriptor in an fd with a new kind. *)
  val replace : t -> Kind.t -> [ `Set of Info.t | `Extend of Info.t ] -> unit
end
