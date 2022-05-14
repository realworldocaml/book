open! Core

include Monad.S with type 'a t = 'a option Deferred0.t (** @inline *)
