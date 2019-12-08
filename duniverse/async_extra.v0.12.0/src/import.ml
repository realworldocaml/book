open! Core_kernel
include Int.Replace_polymorphic_compare
include Async_kernel
include Async_unix
module Kernel_scheduler = Async_kernel_scheduler
module Rpc_kernel = Async_rpc_kernel.Rpc
module Versioned_rpc = Async_rpc_kernel.Versioned_rpc
