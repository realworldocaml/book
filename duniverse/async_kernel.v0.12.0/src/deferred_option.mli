open! Core_kernel

(** @inline *)
include Monad.S with type 'a t = 'a option Deferred0.t
