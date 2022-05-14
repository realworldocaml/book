open! Core
open! Async
open! Import

module type Address = Persistent_connection_kernel.Address
module type Closable = Persistent_connection_kernel.Closable

module type S = sig
  include Persistent_connection_kernel.S

  val create
    :  server_name:string
    -> ?log:Log.t
    (** If [~log] is supplied then all events that would be passed to [on_event] will be
        written there as well, with a "persistent-connection-to" tag value of
        [server_name], which should be the name of the server we are connecting to. *)
    -> ?on_event:('address Event.t -> unit Deferred.t)
    -> ?retry_delay:(unit -> Time.Span.t)
    -> ?random_state:[ `Non_random | `State of Random.State.t ]
    (** If a [~random_state] is supplied, randomization is applied to the result of
        [retry_delay] after each call; if not, no randomization will be applied. The
        default is [`State Random.State.default]. *)
    -> ?time_source:Time_source.t
    -> connect:('address -> conn Or_error.t Deferred.t)
    -> address:(module Address with type t = 'address)
    -> (unit -> 'address Or_error.t Deferred.t)
    -> t

end

module type S_rpc = sig
  include S


  (** Like [create] but for Rpc-like connections (i.e. Async.Rpc and Async.Versioned_rpc)
      where there is an obvious default for [connect] -- with a handful of extra optional
      parameters to pass to the [Rpc.Connection] functions. *)
  val create'
    :  server_name:string
    -> ?log:Log.t
    -> ?on_event:(Host_and_port.t Event.t -> unit Deferred.t)
    -> ?retry_delay:(unit -> Time.Span.t)
    -> ?random_state:[ `Non_random | `State of Random.State.t ]
    -> ?time_source:Time_source.t
    -> ?bind_to_address:Unix.Inet_addr.t
    -> ?implementations:_ Rpc.Connection.Client_implementations.t
    -> ?max_message_size:int
    -> ?make_transport:Rpc.Connection.transport_maker
    -> ?handshake_timeout:Time.Span.t
    -> ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
    -> (unit -> Host_and_port.t Or_error.t Deferred.t)
    -> t
end

module type Persistent_connection = sig
  (** A persistent connection is one that is automatically reestablished whenever lost. *)

  module type Address = Address
  module type Closable = Closable
  module type S = S
  module type S_rpc = S_rpc

  module Make (Conn : Closable) : S with type conn = Conn.t
  module Rpc : S_rpc with type conn = Rpc.Connection.t
  module Versioned_rpc : S_rpc with type conn = Versioned_rpc.Connection_with_menu.t
end
