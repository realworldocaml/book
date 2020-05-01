(** All [Deferred_queue] iteration functions first copy the queue (to a list) and then
    start calling the user function [f].  So, if [f] modifies the queue, that will have no
    effect on the iteration. *)

open! Core_kernel

include Deferred1.Monad_sequence with type 'a t := 'a Queue.t (** @inline *)
