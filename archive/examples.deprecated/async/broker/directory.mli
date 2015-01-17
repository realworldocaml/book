open Core.Std
(** A directory for storing last-values for a basic pub/sub system *)

open Async.Std
open Protocol

type t

(** Creates an empty directory *)
val create : unit -> t

val publish   : t -> Message.t -> unit

(** If the topic is unknown, then None is returned *)
val subscribe : t -> Topic.t -> Message.t Pipe.Reader.t option

(** Creates a dump of the current state of directory *)
val dump : t -> Dump.t
