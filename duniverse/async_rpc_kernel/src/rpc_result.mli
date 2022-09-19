open! Core
open! Async_kernel

type 'a t = 'a Protocol.Rpc_result.t

val uncaught_exn : location:string -> exn -> 'a t
val bin_io_exn : location:string -> exn -> 'a t

val try_with
  :  ?on_background_exception:(exn -> unit)
  -> ?run:[ `Now | `Schedule ]
  -> location:string
  -> (unit -> 'a t Deferred.t)
  -> 'a t Deferred.t

val or_error
  :  rpc_description:Description.t
  -> connection_description:Info.t
  -> connection_close_started:Info.t Deferred.t
  -> 'a t
  -> 'a Or_error.t
