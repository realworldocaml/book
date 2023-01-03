(** Effectful operations using Eio for pure TLS.

    The pure TLS is state and buffer in, state and buffer out.  This
    module uses Eio for communication over the network. *)

(** [Tls_alert] exception received from the other endpoint *)
exception Tls_alert   of Tls.Packet.alert_type

(** [Tls_failure] exception while processing incoming data *)
exception Tls_failure of Tls.Engine.failure

type t = private < Eio.Flow.two_way; .. >

(** {2 Constructors} *)

(** [server_of_flow server flow] is [t], after server-side TLS
    handshake of [flow] using [server] configuration.

    You must ensure a RNG is installed while using TLS, e.g. using [Mirage_crypto_rng_eio].
    Ideally, this would be part of the [server] config so you couldn't forget it,
    but for now you'll get a runtime error if you forget. *)
val server_of_flow : Tls.Config.server -> #Eio.Flow.two_way -> t

(** [client_of_flow client ~host fd] is [t], after client-side
    TLS handshake of [flow] using [client] configuration and [host].

    You must ensure a RNG is installed while using TLS, e.g. using [Mirage_crypto_rng_eio].
    Ideally, this would be part of the [client] config so you couldn't forget it,
    but for now you'll get a runtime error if you forget. *)
val client_of_flow : Tls.Config.client -> ?host:[ `host ] Domain_name.t -> #Eio.Flow.two_way -> t

(** {2 Control of TLS features} *)

(** [reneg ~authenticator ~acceptable_cas ~cert ~drop t] renegotiates the
    session, and blocks until the renegotiation finished.  Optionally, a new
    [authenticator] and [acceptable_cas] can be used.  The own certificate can
    be adjusted by [cert]. If [drop] is [true] (the default),
    application data received before the renegotiation finished is dropped. *)
val reneg :
  ?authenticator:X509.Authenticator.t ->
  ?acceptable_cas:X509.Distinguished_name.t list ->
  ?cert:Tls.Config.own_cert ->
  ?drop:bool ->
  t -> unit

(** [key_update ~request t] updates the traffic key and requests a traffic key
    update from the peer if [request] is provided and [true] (the default).
    This is only supported in TLS 1.3. *)
val key_update : ?request:bool -> t -> unit

(** [epoch t] returns [epoch], which contains information of the
    active session. *)
val epoch : t -> (Tls.Core.epoch_data, unit) result
