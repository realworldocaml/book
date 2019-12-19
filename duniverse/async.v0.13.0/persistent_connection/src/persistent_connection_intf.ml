open! Core
open! Async
open! Import

module type S = sig
  include Persistent_connection_kernel.S

  val create
    :  server_name:string
    -> ?log:Log.t
    (** If [~log] is supplied then all events that would be passed to [on_event] will be
        written there as well, with a "persistent-connection-to" tag value of
        [server_name], which should be the name of the server we are connecting to. *)
    -> ?on_event:(Event.t -> unit Deferred.t)
    -> ?retry_delay:(unit -> Time.Span.t)
    -> ?random_state:Random.State.t
    -> ?time_source:Time_source.t
    -> connect:(address -> conn Or_error.t Deferred.t)
    -> (unit -> address Or_error.t Deferred.t)
    -> t
end

module type T = Persistent_connection_kernel.T

module type Persistent_connection = sig
  (** A persistent connection is one that is automatically reestablished whenever lost. *)

  module type S = S
  module type T = T

  module Make (Conn : T) : S with type conn = Conn.t and type address = Conn.Address.t

  module Rpc : sig
    include S with type conn = Rpc.Connection.t and type address = Host_and_port.t

    (** [create'] is like the [create], but slightly more convenient for constructing
        unembellished rpc connections. *)
    val create'
      :  server_name:string
      -> ?log:Log.t
      -> ?on_event:(Event.t -> unit Deferred.t)
      -> ?retry_delay:(unit -> Time.Span.t)
      -> ?bind_to_address:Unix.Inet_addr.t
      -> ?implementations:_ Rpc.Connection.Client_implementations.t
      -> ?max_message_size:int
      -> ?make_transport:Rpc.Connection.transport_maker
      -> ?handshake_timeout:Time.Span.t
      -> ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
      -> (unit -> Host_and_port.t Or_error.t Deferred.t)
      -> t

  end

  module Versioned_rpc :
    S
    with type conn = Versioned_rpc.Connection_with_menu.t
     and type address = Host_and_port.t
end
