open! Core_kernel

(** @inline *)
include Deferred1.Monad_sequence with type 'a t := 'a Sequence.t
