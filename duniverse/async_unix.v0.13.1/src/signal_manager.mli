(** A signal manager keeps track of a set of signals to be managed and the signal handlers
    for them.  When a signal manager is managing a signal, it installs its own OCaml
    handler for that signal that records delivery of the signal.  It then later, upon
    request, will deliver the signal to all its handlers.

    Once a signal manager starts managing a signal, it never stops. *)

open! Core

type t [@@deriving sexp_of]

include Invariant.S with type t := t

(** [create] creates and returns a signal manager [t].  Whenever a signal that [t] is
    managing is delivered, it will call [thread_safe_notify_signal_delivered] from within
    the OCaml signal handler.  Therefore [thread_safe_notify_signal_delivered] must be
    thread safe. *)
val create : thread_safe_notify_signal_delivered:(unit -> unit) -> t

(** [manage t signal] causes [t] to manage [signal], thus overriding
    [default_sys_behavior] for that signal, and any other OCaml handler for that
    signal. *)
val manage : t -> Signal.t -> unit

(** [is_managing t signal] returns true iff [manage t signal] has been called *)
val is_managing : t -> Signal.t -> bool

(** [install_handler t signals f] causes [t] to manage the handling of [signals], and
    registers [f] to run on every signal in [signals] that is delivered.   It is an
    error if [f] ever raises when it is called. *)
type handler

val install_handler : t -> Signal.t list -> (Signal.t -> unit) -> handler

(** [remove_handler handler] causes the particular [handler] to no longer handle the
    signals it was registered to handle.  The signal manager continues to manage those
    signals, i.e. the OCaml signal handler remains installed, whether or not they still
    have handlers. *)
val remove_handler : t -> handler -> unit

(** [handle_delivered t] runs all signal handlers on the signals that have been
    delivered but not yet handled. *)
val handle_delivered : t -> unit
