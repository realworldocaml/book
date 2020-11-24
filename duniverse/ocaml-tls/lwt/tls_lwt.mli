(** Effectful operations using Lwt for pure TLS.

    The pure TLS is state and buffer in, state and buffer out.  This
    module uses Lwt for communication over the network.

    This module implements a high-level API and a low-level API (in
    {!Unix}).  Most applications should use the high-level API described below. *)

(** [Tls_alert] exception received from the other endpoint *)
exception Tls_alert   of Tls.Packet.alert_type

(** [Tls_failure] exception while processing incoming data *)
exception Tls_failure of Tls.Engine.failure

(** Low-level API *)
module Unix : sig

  (** {1 Unix API} *)

  (** It is the responsibility of the client to handle error
      conditions.  The underlying file descriptors are not closed. *)

  (** Abstract type of a session *)
  type t

  (** {2 Constructors} *)

  (** [server_of_fd server fd] is [t], after server-side TLS
      handshake of [fd] using [server] configuration. *)
  val server_of_fd : Tls.Config.server -> Lwt_unix.file_descr -> t Lwt.t

  (** [client_of_fd client ~host fd] is [t], after client-side
      TLS handshake of [fd] using [client] configuration and [host]. *)
  val client_of_fd : Tls.Config.client -> ?host:string -> Lwt_unix.file_descr -> t Lwt.t

  (** [accept server fd] is [t, sockaddr], after accepting a
      client on [fd] and upgrading to a TLS connection. *)
  val accept  : Tls.Config.server -> Lwt_unix.file_descr -> (t * Lwt_unix.sockaddr) Lwt.t

  (** [connect client (host, port)] is [t], after successful
      connection to [host] on [port] and TLS upgrade. *)
  val connect : Tls.Config.client -> string * int -> t Lwt.t

  (** {2 Common stream operations} *)

  (** [read t buffer] is [length], the number of bytes read into
      [buffer]. *)
  val read   : t -> Cstruct.t      -> int  Lwt.t

  (** [write t buffer] writes the [buffer] to the session. *)
  val write  : t -> Cstruct.t      -> unit Lwt.t

  (** [writev t buffers] writes the [buffers] to the session. *)
  val writev : t -> Cstruct.t list -> unit Lwt.t

  (** [read_bytes t bytes offset len] is [read_bytes], the amount of
      bytes read. *)
  val read_bytes  : t -> Lwt_bytes.t -> int -> int -> int  Lwt.t

  (** [write_bytes t bytes offset length] writes [length] bytes of
      [bytes] starting at [offset] to the session. *)
  val write_bytes : t -> Lwt_bytes.t -> int -> int -> unit Lwt.t

  (** [close t] closes the TLS session by sending a close notify to the peer. *)
  val close_tls : t -> unit Lwt.t

  (** [close t] closes the TLS session and the underlying file descriptor. *)
  val close : t -> unit Lwt.t

  (** [reneg ~authenticator ~acceptable_cas ~cert ~drop t] renegotiates the
      session, and blocks until the renegotiation finished.  Optionally, a new
      [authenticator] and [acceptable_cas] can be used.  The own certificate can
      be adjusted by [cert]. If [drop] is [true] (the default),
      application data received before the renegotiation finished is dropped. *)
  val reneg : ?authenticator:X509.Authenticator.t ->
    ?acceptable_cas:X509.Distinguished_name.t list -> ?cert:Tls.Config.own_cert ->
    ?drop:bool -> t -> unit Lwt.t

  (** [key_update ~request t] updates the traffic key and requests a traffic key
      update from the peer if [request] is provided and [true] (the default).
      This is only supported in TLS 1.3. *)
  val key_update : ?request:bool -> t -> unit Lwt.t

  (** [epoch t] returns [epoch], which contains information of the
      active session. *)
  val epoch  : t -> [ `Ok of Tls.Core.epoch_data | `Error ]
end

(** {1 High-level API} *)

type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

(** [accept_ext server fd] is [(ic, oc), sockaddr], the input
    and output channel from an accepted connection on the given [fd],
    after upgrading to TLS using the [server] configuration. *)
val accept_ext : Tls.Config.server -> Lwt_unix.file_descr ->
  ((ic * oc) * Lwt_unix.sockaddr) Lwt.t

(** [accept own_cert fd] is [(ic, oc), sockaddr], the input and
    output channel from the accepted connection on [fd], using the
    default configuration with the given [own_cert]. *)
val accept : Tls.Config.own_cert -> Lwt_unix.file_descr ->
  ((ic * oc) * Lwt_unix.sockaddr) Lwt.t

(** [connect_ext client (host, port)] is [ic, oc], the input
    and output channel of a TLS connection to [host] on [port] using
    the [client] configuration. *)
val connect_ext : Tls.Config.client -> string * int -> (ic * oc) Lwt.t

(** [connect authenticator (host, port)] is [ic, oc], the input
    and output channel of a TLS connection to [host] on [port] using the
    default configuration and the [authenticator]. *)
val connect : X509_lwt.authenticator -> string * int -> (ic * oc) Lwt.t

(** [of_t t] is [ic, oc], the input and output channel.  [close]
    defaults to [!Unix.close]. *)
val of_t : ?close:(unit -> unit Lwt.t) -> Unix.t -> ic * oc
