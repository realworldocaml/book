(** A publisher for a single topic *)
type t

val create : Message.t -> t
val publish : t -> Message.t -> unit
val subscribe : t -> Message.t Pipe.Reader.t
val num_subscribers : t -> int
val last_message : t -> Message.t
