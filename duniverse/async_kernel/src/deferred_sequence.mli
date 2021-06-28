open! Core_kernel

include Deferred1.Monad_sequence with type 'a t := 'a Sequence.t (** @inline *)
