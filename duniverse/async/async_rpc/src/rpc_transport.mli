open! Core
open! Import

(*_ The [Reader] and [Writer] modules from [Async], renamed to avoid conflicting with
  [Reader] and [Writer] below. *)
module Async_reader := Reader
module Async_writer := Writer

module Reader : sig
  include module type of struct
    include Rpc_kernel.Transport.Reader
  end

  val of_reader : ?max_message_size:int -> Async_reader.t -> t
end

module Writer : sig
  include module type of struct
    include Rpc_kernel.Transport.Writer
  end

  val of_writer : ?max_message_size:int -> Async_writer.t -> t
end

include module type of struct
  include Rpc_kernel.Transport
end
with module Reader := Rpc_kernel.Transport.Reader
with module Writer := Rpc_kernel.Transport.Writer

val of_reader_writer : ?max_message_size:int -> Async_reader.t -> Async_writer.t -> t

val of_fd
  :  ?buffer_age_limit:Async_writer.buffer_age_limit
  -> ?reader_buffer_size:int
  -> max_message_size:int
  -> Fd.t
  -> t

module Tcp : sig
  type transport_maker := Fd.t -> max_message_size:int -> Rpc_kernel.Transport.t

  (** [serve] takes a callback; your callback will be handed a [Rpc.Transport.t] and it's
      your responsibility to create the [Rpc.Connection.t]. The transport will be closed
      when your callback returns; ending your callback with a call to
      [Rpc.Connection.close_finished] is likely appropriate. *)
  val serve
    :  where_to_listen:('address, 'listening_on) Tcp.Where_to_listen.t
    -> ?max_connections:int
    -> ?backlog:int
    -> ?drop_incoming_connections:bool
    -> ?time_source:[> read ] Time_source.T1.t
    -> ?max_message_size:int
    -> ?make_transport:transport_maker
    (** default is [of_fd] (as opposed to [Rpc_transport_low_latency]) *)
    -> ?auth:('address -> bool)
    -> ?on_handler_error:[ `Raise | `Ignore | `Call of 'address -> exn -> unit ]
    (** default is [`Ignore] *)
    -> (client_addr:'address
        -> server_addr:'address
        -> Rpc_kernel.Transport.t
        -> unit Deferred.t)
    -> ('address, 'listening_on) Tcp.Server.t Deferred.t

  (** [serve_inet] is like [serve] but only for inet sockets (not unix sockets), and
      returning the server immediately, without deferreds. *)
  val serve_inet
    :  where_to_listen:(Socket.Address.Inet.t, int) Tcp.Where_to_listen.t
    -> ?max_connections:int
    -> ?backlog:int
    -> ?drop_incoming_connections:bool
    -> ?time_source:[> read ] Time_source.T1.t
    -> ?max_message_size:int
    -> ?make_transport:transport_maker
    -> ?auth:(Socket.Address.Inet.t -> bool)
    -> ?on_handler_error:
         [ `Raise | `Ignore | `Call of Socket.Address.Inet.t -> exn -> unit ]
    -> (client_addr:Socket.Address.Inet.t
        -> server_addr:Socket.Address.Inet.t
        -> Rpc_kernel.Transport.t
        -> unit Deferred.t)
    -> (Socket.Address.Inet.t, int) Tcp.Server.t

  (** [connect ?make_transport where_to_connect ()] connects to the server at
      [where_to_connect]. On success, it returns the transport created using
      [make_transport] and the [Socket.Address.t] that it connected to, otherwise it
      returns the Error.

      It is your responsibility to close the [Transport.t] *)
  val connect
    :  ?max_message_size:int
    -> ?make_transport:transport_maker
    (** default is [of_fd] (as opposed to [Rpc_transport_low_latency]) *)
    -> ?tcp_connect_timeout:Time_ns.Span.t
    -> 'addr Tcp.Where_to_connect.t
    -> (Rpc_kernel.Transport.t * 'addr, Exn.t) Result.t Deferred.t
end
