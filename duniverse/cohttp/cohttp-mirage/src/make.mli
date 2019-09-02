
(** HTTP server. *)
module Server (Flow: Mirage_flow_lwt.S): sig
  include Cohttp_lwt.S.Server with type IO.conn = Flow.flow
  val listen: t -> IO.conn -> unit Lwt.t
end
