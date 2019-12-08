(** The [Server] module implements the full UNIX HTTP server interface,
    including the UNIX-specific functions defined in {!S}. *)

include Cohttp_lwt.S.Server with module IO = Io

val resolve_file : docroot:string -> uri:Uri.t -> string

val respond_file :
  ?headers:Cohttp.Header.t ->
  fname:string -> unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t


(** [create ?timeout ?backlog ?stop ?on_exn ?ctx ?mode t] is a new
   HTTP server.

    When provided, [mode] selects the connection type. By default it
   is using a TCP socket listening on port 8080.

    When provided, [ctx] is the network context to use. By default is
   {!Net.default_ctx}.

    When provided, the [stop] thread will terminate the server if it
   ever becomes determined.

    When provided, [backlog] will limit the number of open
   connections.

    Every connection will be served in a new lightweight thread that
   is invoked via the callback defined in [t]. If the callback raises
   an exception, it is passed to [on_exn] (by default, to a function
   that logs the exceptiom using the {!Logs} library). *)
val create :
  ?timeout:int ->
  ?backlog:int ->
  ?stop:unit Lwt.t ->
  ?on_exn:(exn -> unit) ->
  ?ctx:Net.ctx ->
  ?mode:Conduit_lwt_unix.server -> t -> unit Lwt.t
