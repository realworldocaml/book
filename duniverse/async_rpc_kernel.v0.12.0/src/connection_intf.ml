open Core_kernel
open Async_kernel

(* The reason for defining this module type explicitly is so that we can internally keep
   track of what is and isn't exposed. *)
module type S = sig
  type t [@@deriving sexp_of]

  module Heartbeat_config : sig
    type t [@@deriving sexp, bin_io]

    (** We try to send a heartbeat every [send_every]. If we don't get a heartbeat
        for [timeout], kill the connection. [timeout] is checked every time a heartbeat
        is sent.
    *)
    val create
      :  timeout : Time_ns.Span.t
      -> send_every : Time_ns.Span.t
      -> t
  end

  module Client_implementations : sig
    type nonrec 's t =
      { connection_state : t -> 's
      ; implementations  : 's Implementations.t
      }

    val null : unit -> unit t
  end

  (** Initiate an Rpc connection on the given transport.  [implementations] should be the
      bag of implementations that the calling side implements; it defaults to
      [Implementations.null] (i.e., "I implement no RPCs").

      [connection_state] will be called once, before [create]'s result is determined, on
      the same connection that [create] returns.  Its output will be provided to the
      [implementations] when queries arrive.

      WARNING: If specifying a custom [heartbeat_config], make sure that both ends of the
      Rpc connection use compatible settings for timeout and send frequency. Otherwise,
      your Rpc connections might close unexpectedly.

      [description] can be used to give some extra information about the connection, which
      will then show up in error messages and the connection's sexp. If you have lots of
      connections in your program, this can be useful for distinguishing them.
  *)
  val create
    :  ?implementations    : 's Implementations.t
    -> connection_state    : (t -> 's)
    -> ?handshake_timeout  : Time_ns.Span.t
    -> ?heartbeat_config   : Heartbeat_config.t
    -> ?description        : Info.t
    -> Transport.t
    -> (t, Exn.t) Result.t Deferred.t

  (** As of Feb 2017, the RPC protocol started to contain a magic number so that one can
      identify RPC communication.  The bool returned by [contains_magic_prefix] says
      whether this magic number was observed. *)
  val contains_magic_prefix : bool Bin_prot.Type_class.reader

  val description : t -> Info.t

  (** After [add_heartbeat_callback t f], [f ()] will be called on every subsequent
      heartbeat to [t]. *)
  val add_heartbeat_callback : t -> (unit -> unit) -> unit

  (** [close] starts closing the connection's transport, and returns a deferred that
      becomes determined when its close completes.  It is ok to call [close] multiple
      times on the same [t]; calls subsequent to the initial call will have no effect, but
      will return the same deferred as the original call.

      Before closing the underlying transport's writer, [close] waits for all streaming
      reponses to be [Pipe.upstream_flushed] with a timeout of
      [streaming_responses_flush_timeout].

      The [reason] for closing the connection will be passed to callers of [close_reason].
  *)
  val close
    :  ?streaming_responses_flush_timeout:Time_ns.Span.t (* default: 5 seconds *)
    -> ?reason:Info.t
    -> t
    -> unit Deferred.t

  (** [close_finished] becomes determined after the close of the connection's transport
      completes, i.e. the same deferred that [close] returns.  [close_finished] differs
      from [close] in that it does not have the side effect of initiating a close. *)
  val close_finished : t -> unit Deferred.t

  (** [close_reason ~on_close t] becomes determined when close starts or finishes
      based on [on_close], but additionally returns the reason that the connection was
      closed. *)
  val close_reason : t -> on_close: [`started | `finished] -> Info.t Deferred.t

  (** [is_closed t] returns [true] iff [close t] has been called.  [close] may be called
      internally upon errors or timeouts. *)
  val is_closed : t -> bool

  (** [bytes_to_write] and [flushed] just call the similarly named functions on the
      [Transport.Writer.t] within a connection. *)
  val bytes_to_write : t -> int
  val flushed : t -> unit Deferred.t

  (** [with_close] tries to create a [t] using the given transport.  If a handshake error
      is the result, it calls [on_handshake_error], for which the default behavior is to
      raise an exception.  If no error results, [dispatch_queries] is called on [t].

      After [dispatch_queries] returns, if [server] is None, the [t] will be closed and
      the deferred returned by [dispatch_queries] wil be determined immediately.
      Otherwise, we'll wait until the other side closes the connection and then close [t]
      and determine the deferred returned by [dispatch_queries].

      When the deferred returned by [with_close] becomes determined, [Transport.close] has
      finished.

      NOTE: Because this connection is closed when the [Deferred.t] returned by
      [dispatch_queries] is determined, you should be careful when using this with
      [Pipe_rpc].  For example, simply returning the pipe when you get it will close the
      pipe immediately.  You should instead either use the pipe inside [dispatch_queries]
      and not determine its result until you are done with the pipe, or use a different
      function like [create]. *)
  val with_close
    :  ?implementations    : 's Implementations.t
    -> ?handshake_timeout  : Time_ns.Span.t
    -> ?heartbeat_config   : Heartbeat_config.t
    -> connection_state    : (t -> 's)
    -> Transport.t
    -> dispatch_queries    : (t -> 'a Deferred.t)
    -> on_handshake_error  : [ `Raise | `Call of (Exn.t -> 'a Deferred.t) ]
    -> 'a Deferred.t

  (** Runs [with_close] but dispatches no queries. The implementations are required
      because this function doesn't let you dispatch any queries (i.e., act as a client),
      it would be pointless to call it if you didn't want to act as a server.*)
  val server_with_close
    :  ?handshake_timeout  : Time_ns.Span.t
    -> ?heartbeat_config   : Heartbeat_config.t
    -> Transport.t
    -> implementations     : 's Implementations.t
    -> connection_state    : (t -> 's)
    -> on_handshake_error  : [ `Raise | `Ignore | `Call of (Exn.t -> unit Deferred.t) ]
    -> unit Deferred.t
end
