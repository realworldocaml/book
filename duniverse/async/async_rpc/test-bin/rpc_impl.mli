open Core
open Async
open Rpc

(* Generic wrapper over both the Async.Tcp and Netkit backed RPC implementations *)

module Server : sig
  type t

  val bound_on : t -> int
  val close : t -> unit Deferred.t
end

type t

val make_client
  :  ?heartbeat_config:Connection.Heartbeat_config.t
  -> t
  -> string
  -> int
  -> (Connection.t, Exn.t) result Deferred.t

val with_client
  :  ?heartbeat_config:Connection.Heartbeat_config.t
  -> t
  -> string
  -> int
  -> (Connection.t -> 'a Deferred.t)
  -> ('a, Exn.t) result Deferred.t

val make_server
  :  ?heartbeat_config:Connection.Heartbeat_config.t
  -> ?port:int
  -> implementations:'a Implementations.t
  -> initial_connection_state:(Connection.t -> 'a)
  -> t
  -> Server.t Deferred.t

val spec
  :  unit
  -> (rpc_impl:t -> unit -> unit Deferred.t, unit -> unit Deferred.t) Command.Spec.t
