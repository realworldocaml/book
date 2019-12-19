(** The part of the {{!Async_kernel.Execution_context}[Execution_context]} that determines
    what to do when there is an unhandled exception.

    Every Async computation runs within the context of some monitor, which, when the
    computation is running, is referred to as the "current" monitor.  Monitors are
    arranged in a tree -- when a new monitor is created, it is a child of the current
    monitor.

    If a computation raises an unhandled exception, the behavior depends on whether the
    current monitor is "detached" or "attached".  If the monitor has been "detached", via
    one of the [detach*] functions, then whoever detached it is responsible for dealing
    with the exception.  If the monitor is still attached, then the exception bubbles to
    monitor's parent.  If an exception bubbles to the initial monitor, i.e., the root of
    the monitor tree, that prints an unhandled-exception message and calls exit 1.

    {2 Note about the toplevel monitor}

    It is important to note that in the toplevel monitor, exceptions will only be caught
    in the Async part of a computation.  For example, in:

    {[
      upon (f ()) g ]}

    if [f] raises, the exception will not go to a monitor; it will go to the next caml
    exception handler on the stack.  Any exceptions raised by [g] will be caught by the
    scheduler and propagated to the toplevel monitor.  Because of this it is advised to
    always use [Scheduler.schedule] or [Scheduler.within].  For example:

    {[
      Scheduler.within (fun () -> upon (f ()) g) ]}

    This code will catch an exception in either [f] or [g], and propagate it to the
    monitor.

    This is only relevant to the toplevel monitor because if you create another monitor
    and you wish to run code within it you have no choice but to use [Scheduler.within].
    [try_with] creates its own monitor and uses [Scheduler.within], so it does not have
    this problem. *)

open! Core_kernel
module Deferred = Deferred1

type t = Monitor0.t [@@deriving sexp_of]

include Invariant.S with type t := t

type 'a with_optional_monitor_name =
  ?here:Source_code_position.t -> ?info:Info.t -> ?name:string -> 'a

(** [create ()] returns a new monitor whose parent is the current monitor. *)
val create : (unit -> t) with_optional_monitor_name

(** [name t] returns the name of the monitor, or a unique id if no name was supplied to
    [create]. *)
val name : t -> Info.t

val parent : t -> t option
val depth : t -> int

(** [current ()] returns the current monitor. *)
val current : unit -> t


(** [detach t] detaches [t] so that errors raised to [t] are not passed to [t]'s parent
    monitor.  If those errors aren't handled in some other way, then they will effectively
    be ignored.  One should usually use [detach_and_iter_errors] so that errors are not
    ignored. *)
val detach : t -> unit


(** [detach_and_iter_errors t ~f] detaches [t] and passes to [f] all subsequent errors
    that reach [t], stopping iteration if [f] raises an exception.  An exception raised by
    [f] is sent to the monitor in effect when [detach_and_iter_errors] was called. *)
val detach_and_iter_errors : t -> f:(exn -> unit) -> unit

(** [detach_and_get_next_error t] detaches [t] and returns a deferred that becomes
    determined with the next error that reaches [t] (possibly never). *)
val detach_and_get_next_error : t -> exn Deferred.t

(** [detach_and_get_error_stream t] detaches [t] and returns a stream of all subsequent
    errors that reach [t].

    [Stream.iter (detach_and_get_error_stream t) ~f] is equivalent to
    [detach_and_iter_errors t ~f]. *)
val detach_and_get_error_stream : t -> exn Tail.Stream.t

(** [get_next_error t] returns a deferred that becomes determined the next time [t] gets
    an error, if ever.  Calling [get_next_error t] does not detach [t], and if no other
    call has detached [t], then errors will still bubble up the monitor tree. *)
val get_next_error : t -> exn Deferred.t

(** [extract_exn exn] extracts the exn from an error exn that comes from a monitor. If it
    is not supplied such an error exn, it returns the exn itself.  It removes the
    backtrace from the error (see discussion in [try_with]). *)
val extract_exn : exn -> exn

(** [has_seen_error t] returns true iff the monitor has ever seen an error. *)
val has_seen_error : t -> bool

(** [send_exn t exn ?backtrace] sends the exception [exn] as an error to be handled by
    monitor [t].  By default, the error will not contain a backtrace.  However, the caller
    can supply one using [`This], or use [`Get] to request that [send_exn] obtain one
    using [Backtrace.Exn.most_recent ()]. *)
val send_exn : t -> ?backtrace:[ `Get | `This of Backtrace.t ] -> exn -> unit

(** [try_with f] runs [f ()] in a monitor and returns the result as [Ok x] if [f] finishes
    normally, or returns [Error e] if there is an exception.  It either runs [f] now, if
    [run = `Now], or schedules a job to run [f], if [run = `Schedule].  Once a result is
    returned, subsequent exceptions raised to the monitor are handled according to [rest]:

    - [`Log]: Logged to a global error log (cannot raise).
    - [`Raise]: Reraised to the monitor of [try_with]'s caller.
    - [`Call f]: Passed to [f] within the context of the caller of [try_with]'s monitor.

    The [name] argument is used to give a name to the monitor the computation will be
    running in.  This name will appear when printing errors.

    [try_with] runs [f ()] in a new monitor [t] that has no parent.  This works because
    [try_with] calls [detach_and_get_error_stream t] and explicitly handles all errors
    sent to [t].  No errors would ever implicitly propagate to [t]'s parent, although
    [try_with] will explicitly send them to [t]'s parent with [rest = `Raise].

    If [extract_exn = true], then in an [Error exn] result, the [exn] will be the actual
    exception raised by the computation.  If [extract_exn = false], then the [exn] will
    include additional information, like the monitor and backtrace. *)
val try_with
  : (?extract_exn:bool (** default is [false] *)
     -> ?run:[ `Now | `Schedule ] (** default is [`Schedule] *)
     -> ?rest:[ `Log | `Raise | `Call of exn -> unit ] (** default is [`Log] *)
     -> (unit -> 'a Deferred.t)
     -> ('a, exn) Result.t Deferred.t)
      with_optional_monitor_name


(** [try_with_or_error] is like [try_with] but returns ['a Or_error.t Deferred.t]
    instead of [('a,exn) Result.t Deferred.t].  More precisely:

    {[
      try_with_or_error f ?extract_exn
      = try_with f ?extract_exn ~run:`Now ~rest:`Log >>| Or_error.of_exn_result ]}

    [~run:`Now] is different from [try_with]'s default, [~run:`Schedule].  Based on
    experience, we think [~run:`Now] is a better behavior. *)
val try_with_or_error
  : (?extract_exn:bool (** default is [false] *)
     -> (unit -> 'a Deferred.t)
     -> 'a Or_error.t Deferred.t)
      with_optional_monitor_name

(** [try_with_join_or_error f = try_with_or_error f >>| Or_error.join]. *)
val try_with_join_or_error
  : (?extract_exn:bool (** default is [false] *)
     -> (unit -> 'a Or_error.t Deferred.t)
     -> 'a Or_error.t Deferred.t)
      with_optional_monitor_name

(** [handle_errors ?name f handler] runs [f ()] inside a new monitor with the optionally
    supplied name, and calls [handler error] on every error raised to that monitor.  Any
    error raised by [handler] goes to the monitor in effect when [handle_errors] was
    called.

    Errors that are raised after [f ()] becomes determined will still be sent to
    [handler], i.e., the new monitor lives as long as jobs created by [f] live. *)
val handle_errors
  : ((unit -> 'a Deferred.t) -> (exn -> unit) -> 'a Deferred.t)
      with_optional_monitor_name

(** [catch_stream ?name f] runs [f ()] inside a new monitor [m] and returns the stream of
    errors raised to [m]. *)
val catch_stream : ((unit -> unit) -> exn Tail.Stream.t) with_optional_monitor_name

(** [catch ?name f] runs [f ()] inside a new monitor [m] and returns the first error
    raised to [m]. *)
val catch : ((unit -> unit) -> exn Deferred.t) with_optional_monitor_name

(** [catch_error ?name f] runs [f ()] inside of a new monitor [m] and returns the first
    error raised to [m]. *)
val catch_error : ((unit -> unit) -> Error.t Deferred.t) with_optional_monitor_name

(** [protect f ~finally] runs [f ()] and then [finally] regardless of the success or
    failure of [f].  It re-raises any exception thrown by [f] or returns whatever [f]
    returned.

    The [name] argument is used to give a name to the monitor the computation will be
    running in.  This name will appear when printing the errors. *)
val protect
  : (?extract_exn:bool (** default is [false] *)
     -> ?run:[ `Now | `Schedule ] (** default is [`Schedule] *)
     -> (unit -> 'a Deferred.t)
     -> finally:(unit -> unit Deferred.t)
     -> 'a Deferred.t)
      with_optional_monitor_name

(** This is the initial monitor and is the root of the monitor tree.  Unhandled exceptions
    are raised to this monitor. *)
val main : t

module Expert : sig
  (** [try_with_log_exn] is called by [try_with] when an exception is raised to a
      [try_with] that already returned.  [Async_unix] sets it to a function that logs. *)
  val try_with_log_exn : (exn -> unit) ref
end

module Exported_for_scheduler : sig
  type 'a with_options = ?monitor:t -> ?priority:Priority.t -> 'a

  val within' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
  val within : ((unit -> unit) -> unit) with_options
  val within_v : ((unit -> 'a) -> 'a option) with_options
  val schedule' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
  val schedule : ((unit -> unit) -> unit) with_options
  val within_context : Execution_context.t -> (unit -> 'a) -> ('a, unit) Result.t
  val preserve_execution_context : ('a -> unit) -> ('a -> unit) Staged.t

  val preserve_execution_context'
    :  ('a -> 'b Deferred.t)
    -> ('a -> 'b Deferred.t) Staged.t
end
