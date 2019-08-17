(** The context in which an Async job runs. *)

open! Core_kernel
open! Import

type t = Types.Execution_context.t =
  { monitor : Monitor0.t
  ; priority : Priority.t
  ; local_storage : Univ_map.t
  ; backtrace_history : Backtrace.t list
  }
[@@deriving fields, sexp_of]

include Invariant.S with type t := t

val main : t

val create_like
  :  ?monitor:Monitor0.t
  -> ?priority:Priority.t
  -> ?local_storage:Univ_map.t
  -> t
  -> t

val find_local : t -> 'a Univ_map.Key.t -> 'a option
val with_local : t -> 'a Univ_map.Key.t -> 'a option -> t
val record_backtrace : t -> t
