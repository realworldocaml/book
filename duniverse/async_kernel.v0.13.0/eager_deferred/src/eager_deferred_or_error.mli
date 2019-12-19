open! Core_kernel
open! Async_kernel
open! Import

include
  Eager_deferred_intf.Eager_deferred_or_error with type 'a deferred := 'a Deferred.t
