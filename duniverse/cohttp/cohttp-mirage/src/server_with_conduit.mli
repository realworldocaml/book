
(** HTTP server with conduit. *)
include Cohttp_lwt.S.Server with type IO.conn = Conduit_mirage.Flow.flow
val connect:
  Conduit_mirage.t ->
  (Conduit_mirage.server -> t -> unit Lwt.t) Lwt.t
