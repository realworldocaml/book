(* A logging infrastructure.  Before set_logfile is called, errors are logged to stderr *)

open Core.Std

module Rotation_schedule : sig
  type t = | Bytes of int
           | No_rotation
end


(** So you can enable or disable the logger.  When disabled, the string inputs are not
    forced, which reduces the cost of logging. *)
val set_enabled : bool -> unit

(** Adds a new logger to the list of logging modules *)
val add_logger
  :  (module Log_destination.S with type config = 'config)
  -> 'config
  -> Rotation_schedule.t
  -> Time.t
  -> unit Or_error.t

val log : Time.t -> string Lazy.t -> unit

