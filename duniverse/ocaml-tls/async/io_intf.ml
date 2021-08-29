open! Core
open! Async

module type Fd = sig
  type t

  val read : t -> Cstruct.t -> [ `Ok of int | `Eof ] Deferred.Or_error.t
  val write_full : t -> Cstruct.t -> unit Deferred.Or_error.t
end

module type S = sig
  module Fd : Fd

  (** Abstract type of a session *)
  type t

  (** {2 Constructors} *)

  (** [server_of_fd server fd] is [t], after server-side TLS
      handshake of [fd] using [server] configuration. *)
  val server_of_fd : Tls.Config.server -> Fd.t -> t Deferred.Or_error.t

  (** [client_of_fd client ~host fd] is [t], after client-side
      TLS handshake of [fd] using [client] configuration and [host]. *)
  val client_of_fd
    :  Tls.Config.client
    -> ?host:[ `host ] Domain_name.t
    -> Fd.t
    -> t Deferred.Or_error.t

  (** {2 Common stream operations} *)

  (** [read t buffer] is [length], the number of bytes read into
      [buffer]. *)
  val read : t -> Cstruct.t -> int Deferred.Or_error.t

  (** [writev t buffers] writes the [buffers] to the session. *)
  val writev : t -> Cstruct.t list -> unit Deferred.Or_error.t

  (** [close t] closes the TLS session by sending a close notify to the peer. *)
  val close_tls : t -> unit Deferred.Or_error.t

  (** [reneg ~authenticator ~acceptable_cas ~cert ~drop t] renegotiates the
      session, and blocks until the renegotiation finished.  Optionally, a new
      [authenticator] and [acceptable_cas] can be used.  The own certificate can
      be adjusted by [cert]. If [drop] is [true] (the default),
      application data received before the renegotiation finished is dropped. *)
  val reneg
    :  ?authenticator:X509.Authenticator.t
    -> ?acceptable_cas:X509.Distinguished_name.t list
    -> ?cert:Tls.Config.own_cert
    -> ?drop:bool
    -> t
    -> unit Deferred.Or_error.t

  (** [key_update ~request t] updates the traffic key and requests a traffic key
      update from the peer if [request] is provided and [true] (the default).
      This is only supported in TLS 1.3. *)
  val key_update : ?request:bool -> t -> unit Deferred.Or_error.t

  (** [epoch t] returns [epoch], which contains information of the
      active session. *)
  val epoch : t -> Tls.Core.epoch_data Or_error.t
end
