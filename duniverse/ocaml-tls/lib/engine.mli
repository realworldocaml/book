(** Transport layer security

    [TLS] is an implementation of
    {{:https://en.wikipedia.org/wiki/Transport_Layer_Security}transport
    layer security} in OCaml.  TLS is a widely used security protocol
    which establishes an end-to-end secure channel (with optional
    (mutual) authentication) between two endpoints.  It uses TCP/IP as
    transport.  This library supports all three versions of TLS:
    {{:https://tools.ietf.org/html/rfc5246}1.2, RFC5246},
    {{:https://tools.ietf.org/html/rfc4346}1.1, RFC4346}, and
    {{:https://tools.ietf.org/html/rfc2246}1.0, RFC2246}.  SSL, the
    previous protocol definition, is not supported.

    TLS is algorithmically agile: protocol version, key exchange
    algorithm, symmetric cipher, and message authentication code are
    negotiated upon connection.

    This library implements several extensions of TLS,
    {{:https://tools.ietf.org/html/rfc3268}AES ciphers},
    {{:https://tools.ietf.org/html/rfc4366}TLS extensions} (such as
    server name indication, SNI),
    {{:https://tools.ietf.org/html/rfc5746}Renegotiation extension},
    {{:https://tools.ietf.org/html/rfc7627}Session Hash and Extended
    Master Secret Extension}.

    This library does not contain insecure cipher suites (such as
    single DES, export ciphers, ...).  It does not expose the server
    time in the server random, requires secure renegotiation.

    This library consists of a core, implemented in a purely
    functional matter ({!Engine}, this module), and effectful parts:
    {!Tls_lwt} and {!Tls_mirage}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)


(** {1 Abstract state type} *)

(** The abstract type of a TLS state. *)
type state

(** {1 Constructors} *)

(** [client client] is [tls * out] where [tls] is the initial state,
    and [out] the initial client hello *)
val client : Config.client -> (state * Cstruct.t)

(** [server server] is [tls] where [tls] is the initial server
    state *)
val server : Config.server -> state

(** {1 Protocol failures} *)

(** failures which can be mitigated by reconfiguration *)
type error = [
  | `AuthenticationFailure of X509.Validation.validation_error
  | `NoConfiguredCiphersuite of Ciphersuite.ciphersuite list
  | `NoConfiguredVersions of Core.tls_version list
  | `NoConfiguredSignatureAlgorithm of Core.signature_algorithm list
  | `NoMatchingCertificateFound of string
  | `NoCertificateConfigured
  | `CouldntSelectCertificate
]

type client_hello_errors = [
  | `EmptyCiphersuites
  | `NotSetCiphersuites of Packet.any_ciphersuite list
  | `NoSupportedCiphersuite of Packet.any_ciphersuite list
  | `NotSetExtension of Core.client_extension list
  | `HasSignatureAlgorithmsExtension
  | `NoSignatureAlgorithmsExtension
  | `NoGoodSignatureAlgorithms of Core.signature_algorithm list
  | `NoKeyShareExtension
  | `NoSupportedGroupExtension
  | `NotSetSupportedGroup of Packet.named_group list
  | `NotSetKeyShare of (Packet.named_group * Cstruct.t) list
  | `NotSubsetKeyShareSupportedGroup of (Packet.named_group list * (Packet.named_group * Cstruct.t) list)
  | `Has0rttAfterHRR
  | `NoCookie
]

(** failures from received garbage or lack of features *)
type fatal = [
  | `NoSecureRenegotiation
  | `NoSupportedGroup
  | `NoVersions of Core.tls_any_version list
  | `ReaderError of Reader.error
  | `NoCertificateReceived
  | `NoCertificateVerifyReceived
  | `NotRSACertificate
  | `NotRSASignature
  | `KeyTooSmall
  | `RSASignatureMismatch
  | `RSASignatureVerificationFailed
  | `UnsupportedSignatureScheme
  | `HashAlgorithmMismatch
  | `BadCertificateChain
  | `MACMismatch
  | `MACUnderflow
  | `RecordOverflow of int
  | `UnknownRecordVersion of int * int
  | `UnknownContentType of int
  | `CannotHandleApplicationDataYet
  | `NoHeartbeat
  | `BadRecordVersion of Core.tls_any_version
  | `BadFinished
  | `HandshakeFragmentsNotEmpty
  | `InsufficientDH
  | `InvalidDH
  | `InvalidRenegotiation
  | `InvalidClientHello of client_hello_errors
  | `InvalidServerHello
  | `InvalidRenegotiationVersion of Core.tls_version
  | `InappropriateFallback
  | `UnexpectedCCS
  | `UnexpectedHandshake of Core.tls_handshake
  | `InvalidCertificateUsage
  | `InvalidCertificateExtendedUsage
  | `InvalidSession
  | `NoApplicationProtocol
  | `HelloRetryRequest
  | `InvalidMessage
  | `Toomany0rttbytes
  | `MissingContentType
  | `Downgrade12
  | `Downgrade11
  | `UnsupportedKeyExchange
]

(** type of failures *)
type failure = [
  | `Error of error
  | `Fatal of fatal
]

(** [alert_of_failure failure] is [alert], the TLS alert type for this failure. *)
val alert_of_failure : failure -> Packet.alert_type

(** [string_of_failure failure] is [string], the string representation of the [failure]. *)
val string_of_failure : failure -> string

(** [failure_of_sexp sexp] is [failure], the unmarshalled [sexp]. *)
val failure_of_sexp : Sexplib.Sexp.t -> failure

(** [sexp_of_failure failure] is [sexp], the marshalled [failure]. *)
val sexp_of_failure : failure -> Sexplib.Sexp.t

(** {1 Protocol handling} *)

(** result type of {!handle_tls}: either failed to handle the incoming
    buffer ([`Fail]) with {!failure} and potentially a message to send
    to the other endpoint, or sucessful operation ([`Ok]) with a new
    {!state}, an end of file ([`Eof]), or an incoming ([`Alert]).
    Possibly some [`Response] to the other endpoint is needed, and
    potentially some [`Data] for the application was received. *)
type ret = [
  | `Ok of [ `Ok of state | `Eof | `Alert of Packet.alert_type ]
         * [ `Response of Cstruct.t option ]
         * [ `Data of Cstruct.t option ]
  | `Fail of failure * [ `Response of Cstruct.t ]
]

(** [handle_tls state buffer] is [ret], depending on incoming [state]
    and [buffer], the result is the appropriate {!ret} *)
val handle_tls           : state -> Cstruct.t -> ret

(** [can_handle_appdata state] is a predicate which indicates when the
    connection has already completed a handshake. *)
val can_handle_appdata    : state -> bool

(** [handshake_in_progrss state] is a predicate which indicates whether there
    is a handshake in progress or scheduled. *)
val handshake_in_progress : state -> bool

(** [send_application_data tls outs] is [(tls' * out) option] where
    [tls'] is the new tls state, and [out] the cstruct to send over the
    wire (encrypted [outs]). *)
val send_application_data : state -> Cstruct.t list -> (state * Cstruct.t) option

(** [send_close_notify tls] is [tls' * out] where [tls'] is the new
    tls state, and out the (possible encrypted) close notify alert. *)
val send_close_notify     : state -> state * Cstruct.t

(** [reneg ~authenticator ~acceptable_cas ~cert tls] initiates a renegotation on
    [tls], using the provided [authenticator]. It is [tls' * out] where [tls']
    is the new tls state, and [out] either a client hello or hello request
    (depending on which communication endpoint [tls] is). *)
val reneg : ?authenticator:X509.Authenticator.t ->
  ?acceptable_cas:X509.Distinguished_name.t list -> ?cert:Config.own_cert ->
  state -> (state * Cstruct.t) option

(** [key_update ~request state] initiates a KeyUpdate (TLS 1.3 only). If
    [request] is provided and [true] (the default), the KeyUpdate message
    contains a request that the peer should update their traffic key as well. *)
val key_update : ?request:bool -> state -> (state * Cstruct.t, failure) result

(** {1 Session information} *)

(** polymorphic variant of session information.  The first variant
    [`InitialEpoch] will only be used for TLS states without completed
    handshake.  The second variant, [`Epoch], contains actual session
    data. *)
type epoch = [
  | `InitialEpoch
  | `Epoch of Core.epoch_data
]

(** [epoch_of_sexp sexp] is [epoch], the unmarshalled [sexp]. *)
val epoch_of_sexp : Sexplib.Sexp.t -> epoch

(** [sexp_of_epoch epoch] is [sexp], the marshalled [epoch]. *)
val sexp_of_epoch : epoch -> Sexplib.Sexp.t

(** [epoch state] is [epoch], which contains the session
    information. *)
val epoch : state -> epoch
