open Core

(** Configuration of the TLS stack *)

(** {1 Config type} *)

(** certificate chain and private key of the first certificate *)
type certchain = Cert.t list * Mirage_crypto_pk.Rsa.priv

(** polymorphic variant of own certificates *)
type own_cert = [
  | `None
  | `Single of certchain
  | `Multiple of certchain list
  | `Multiple_default of certchain * certchain list
]

type session_cache = SessionID.t -> epoch_data option

(** configuration parameters *)
type config = private {
  ciphers           : Ciphersuite.ciphersuite list ; (** ordered list (regarding preference) of supported cipher suites *)
  protocol_versions : tls_version * tls_version ; (** supported protocol versions (min, max) *)
  hashes            : Mirage_crypto.Hash.hash list ; (** ordered list of supported hash algorithms (regarding preference) *)
  use_reneg         : bool ; (** endpoint should accept renegotiation requests *)
  authenticator     : X509.Authenticator.t option ; (** optional X509 authenticator *)
  peer_name         : string option ; (** optional name of other endpoint (used for SNI RFC4366) *)
  own_certificates  : own_cert ; (** optional default certificate chain and other certificate chains *)
  acceptable_cas    : X509.Distinguished_name.t list ; (** ordered list of acceptable certificate authorities *)
  session_cache     : session_cache ;
  cached_session    : epoch_data option ;
  alpn_protocols    : string list ; (** optional ordered list of accepted alpn_protocols *)
}

val config_of_sexp : Sexplib.Sexp.t -> config
val sexp_of_config : config -> Sexplib.Sexp.t

(** opaque type of a client configuration *)
type client

val client_of_sexp : Sexplib.Sexp.t -> client
val sexp_of_client : client -> Sexplib.Sexp.t

(** opaque type of a server configuration *)
type server

val server_of_sexp : Sexplib.Sexp.t -> server
val sexp_of_server : server -> Sexplib.Sexp.t

(** {1 Constructors} *)

(** [client authenticator ?peer_name ?ciphers ?version ?hashes ?reneg ?certificates ?alpn_protocols] is
    [client] configuration with the given parameters.
    @raise Invalid_argument if the configuration is invalid *)
val client :
  authenticator   : X509.Authenticator.t ->
  ?peer_name      : string ->
  ?ciphers        : Ciphersuite.ciphersuite list ->
  ?version        : tls_version * tls_version ->
  ?hashes         : Mirage_crypto.Hash.hash list ->
  ?reneg          : bool ->
  ?certificates   : own_cert ->
  ?cached_session : epoch_data ->
  ?alpn_protocols : string list ->
  unit -> client

(** [server ?ciphers ?version ?hashes ?reneg ?certificates ?acceptable_cas ?authenticator ?alpn_protocols]
    is [server] configuration with the given parameters.
    @raise Invalid_argument if the configuration is invalid *)
val server :
  ?ciphers        : Ciphersuite.ciphersuite list ->
  ?version        : tls_version * tls_version ->
  ?hashes         : Mirage_crypto.Hash.hash list ->
  ?reneg          : bool ->
  ?certificates   : own_cert ->
  ?acceptable_cas : X509.Distinguished_name.t list ->
  ?authenticator  : X509.Authenticator.t ->
  ?session_cache  : session_cache ->
  ?alpn_protocols : string list ->
  unit -> server

(** [peer client name] is [client] with [name] as [peer_name] *)
val peer : client -> string -> client

(** {1 Note on ALPN protocol selection}

    Both {!val:client} and {!val:server} constructors accept an [alpn_protocols] list. The list for server
    should be given in a descending order of preference. In the case of protocol selection, the server will
    iterate its list and select the first element that the client's list also advertises.

    For example, if the client advertises [["foo"; "bar"; "baz"]] and the server has [["bar"; "foo"]],
    ["bar"] will be selected as the protocol of the handshake. *)

(** {1 Utility functions} *)

(** [default_hashes] is a list of hash algorithms used by default *)
val default_hashes  : Mirage_crypto.Hash.hash list

(** [supported_hashes] is a list of supported hash algorithms by this library *)
val supported_hashes  : Mirage_crypto.Hash.hash list

(** [min_dh_size] is minimal diffie hellman group size in bits (currently 1024) *)
val min_dh_size : int

(** [dh_group] is the default Diffie-Hellman group (currently the
ffdhe2048 group from
{{:https://www.ietf.org/id/draft-ietf-tls-negotiated-ff-dhe-10.txt}Negotiated
Finite Field Diffie-Hellman Ephemeral Parameters for TLS}) *)
val dh_group : Mirage_crypto_pk.Dh.group

(** [min_rsa_key_size] is minimal RSA modulus key size in bits (currently 1024) *)
val min_rsa_key_size : int

(** Cipher selection *)
module Ciphers : sig

  open Ciphersuite

  (** Cipher selection related utilities. *)

  (** {1 Cipher selection} *)

  val default : ciphersuite list
  (** [default] is a list of ciphersuites this library uses by default. *)

  val supported : ciphersuite list
  (** [supported] is a list of ciphersuites this library supports
      (larger than [default]). *)

  val fs : ciphersuite list
  (** [fs] is a list of ciphersuites which provide forward secrecy
      (sublist of [default]). *)

  val fs_of : ciphersuite list -> ciphersuite list
  (** [fs_of ciphers] selects all ciphersuites which provide forward
      secrecy from [ciphers]. *)
end

(** {1 Internal use only} *)

(** [of_client client] is a client configuration for [client] *)
val of_client : client -> config

(** [of_server server] is a server configuration for [server] *)
val of_server : server -> config

(** [with_authenticator config auth] is [config] with [auth] as [authenticator] *)
val with_authenticator : config -> X509.Authenticator.t -> config

(** [with_own_certificates config cert] is [config] with [cert] as [own_cert] *)
val with_own_certificates : config -> own_cert -> config

(** [with_acceptable_cas config cas] is [config] with [cas] as [accepted_cas] *)
val with_acceptable_cas : config -> X509.Distinguished_name.t list -> config
