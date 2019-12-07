open! Core_kernel
open! Async_kernel

include module type of struct include Persistent_connection_kernel end

module Rpc : S
  with type conn    = Rpc.Connection.t
   and type address = Host_and_port.t

module Versioned_rpc : S
  with type conn    = Versioned_rpc.Connection_with_menu.t
   and type address = Host_and_port.t
