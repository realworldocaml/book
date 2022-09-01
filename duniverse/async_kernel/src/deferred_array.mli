open! Core

include Deferred1.Monad_sequence with type 'a t := 'a array (** @inline *)
