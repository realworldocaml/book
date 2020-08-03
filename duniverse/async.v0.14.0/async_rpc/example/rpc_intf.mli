open! Core
open! Async

(** Query for grabbing a unique ID *)
val get_unique_id : (unit, int) Rpc.Rpc.t

(** Query for setting the counter used to generate unique IDs *)
val set_id_counter : (int, unit) Rpc.Rpc.t

(** This is a deprecated query, no longer supported by the server *)
val set_id_counter_v0 : (int * int, unit) Rpc.Rpc.t

(** For getting a stream updating the counter values *)
val counter_values : (unit, int, unit) Rpc.Pipe_rpc.t
