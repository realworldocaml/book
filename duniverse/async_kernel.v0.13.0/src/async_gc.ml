open Core_kernel
include Gc

(** [add_finalizer f x] is like [Gc.finalise f x], except that the finalizer is guaranteed
    to run as an Async job (i.e. without interrupting other Async jobs).  Unprotected use
    of [Caml.Gc.finalise] or [Core.Gc.add_finalizer] in Async programs is wrong, because
    the finalizers won't hold the async lock, and thus could interleave arbitrarily with
    async jobs. *)
let add_finalizer heap_block f = Scheduler.(add_finalizer (t ())) heap_block f

let add_finalizer_exn heap_block f = Scheduler.(add_finalizer_exn (t ())) heap_block f
let add_finalizer_last heap_block f = Scheduler.(add_finalizer_last (t ())) heap_block f

let add_finalizer_last_exn heap_block f =
  Scheduler.(add_finalizer_last_exn (t ())) heap_block f
;;

module Alarm = struct
  module Alarm = Gc.Expert.Alarm

  type t = Alarm.t [@@deriving sexp_of]

  let create f = Scheduler.(create_alarm (t ())) f
  let delete = Alarm.delete
end
