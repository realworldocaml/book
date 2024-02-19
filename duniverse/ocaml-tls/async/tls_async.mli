open! Core
open! Async

(** Low-level API for working with TLS sessions.
    Most applications should use the high-level API below *)
module Session = Session

(** Helper functions for [Async_unix]-specific IO operations commonly used with X509
    certificates, such as loading from a Unix filesystem *)
module X509_async = X509_async

(** [listen] creates a [Tcp.Server.t] with the requested parameters, including those
    specified in [Tls.Config.server]. The handler function exposes the low-level
    [Session.t] to accommodate cases like interrogating a client certificate *)
val listen
  :  ?buffer_age_limit:Writer.buffer_age_limit
  -> ?max_connections:int (** defaults to [10_000]. *)
  -> ?max_accepts_per_batch:int (** defaults to [1]. *)
  -> ?backlog:int (** defaults to [64]. *)
  -> ?socket:([ `Unconnected ], ([< Socket.Address.t ] as 'address)) Socket.t
  -> on_handler_error:[ `Call of 'address -> exn -> unit | `Ignore | `Raise ]
  -> Tls.Config.server
  -> ('address, 'listening_on) Tcp.Where_to_listen.t
  -> ('address -> Session.t -> Reader.t -> Writer.t -> unit Deferred.t)
  -> ('address, 'listening_on) Tcp.Server.t Deferred.t

type 'a io_handler = Reader.t -> Writer.t -> 'a Deferred.t
type 'a tls_handler = Session.t -> 'a io_handler

(** [upgrade_server_handler] is what [listen] calls to handle each client.
    It is exposed so that low-level end-users of the library can use tls-async
    inside of code that manages Tcp services directly.

    The [tls_handler] argument will be called with the client Tls session,
    reader and writer to be used for cleartext data.

    The outer [reader] and [writer] will read encrypted data from and write
    encrypted data to the connected socket. *)
val upgrade_server_handler
  : config:Tls.Config.server
  -> 'a tls_handler
  -> 'a io_handler

(** [connect] behaves similarly to [Tcp.connect], exposing a cleartext reader and writer.
    Callers should ensure they close the [Writer.t] and wait for the [unit Deferred.t]
    returned by [`Closed_and_flushed_downstream] to completely shut down the TLS connection

    [host] is used for peer name verification and should generally be provided. Passing
    [None] will disable peer name verification unless [peer_name] was provided in the
    [Tls.Config.client]. If both are present [host] overwrites [peer_name].
*)
val connect
  :  ?socket:([ `Unconnected ], 'addr) Socket.t
  -> (Tls.Config.client
      -> 'addr Tcp.Where_to_connect.t
      -> host:[ `host ] Domain_name.t option
      -> (Session.t * Reader.t * Writer.t) Deferred.Or_error.t)
       Tcp.with_connect_options
