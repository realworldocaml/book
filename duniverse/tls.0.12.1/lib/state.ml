(* Defines all high-level datatypes for the TLS library. It is opaque to clients
 of this library, and only used from within the library. *)

open Sexplib
open Sexplib.Conv

open Core
open Mirage_crypto

type hmac_key = Cstruct.t

(* initialisation vector style, depending on TLS version *)
type iv_mode =
  | Iv of Cstruct_sexp.t  (* traditional CBC (reusing last cipherblock) *)
  | Random_iv        (* TLS 1.1 and higher explicit IV (we use random) *)
  [@@deriving sexp]

type 'k cbc_cipher    = (module Cipher_block.S.CBC with type key = 'k)
type 'k cbc_state = {
  cipher         : 'k cbc_cipher ;
  cipher_secret  : 'k ;
  iv_mode        : iv_mode ;
  hmac           : Hash.hash ;
  hmac_secret    : hmac_key
}

type nonce = Cstruct.t

type 'k aead_cipher =
  | CCM of (module Cipher_block.S.CCM with type key = 'k)
  | GCM of (module Cipher_block.S.GCM with type key = 'k)

type 'k aead_state = {
  cipher         : 'k aead_cipher ;
  cipher_secret  : 'k ;
  nonce          : nonce
}

(* state of a symmetric cipher *)
type cipher_st =
  | CBC    : 'k cbc_state -> cipher_st
  | AEAD   : 'k aead_state -> cipher_st

(* Sexplib stubs -- rethink how to play with crypto. *)
let sexp_of_cipher_st = function
  | CBC _    -> Sexp.Atom "<cbc-state>"
  | AEAD _   -> Sexp.Atom "<aead-state>"

let cipher_st_of_sexp =
  Conv.of_sexp_error "cipher_st_of_sexp: not implemented"
(* *** *)

(* context of a TLS connection (both in and out has each one of these) *)
type crypto_context = {
  sequence  : int64 ; (* sequence number *)
  cipher_st : cipher_st ; (* cipher state *)
} [@@deriving sexp]

(* the raw handshake log we need to carry around *)
type hs_log = Cstruct_sexp.t list [@@deriving sexp]

type dh_secret = [
  | `Fiat of Fiat_p256.secret
  | `Hacl of Hacl_x25519.secret
  | `Mirage_crypto of Mirage_crypto_pk.Dh.secret
]
let sexp_of_dh_secret _ = Sexp.Atom "dh_secret"
let dh_secret_of_sexp = Conv.of_sexp_error "dh_secret_of_sexp: not implemented"


(* a collection of client and server verify bytes for renegotiation *)
type reneg_params = Cstruct_sexp.t * Cstruct_sexp.t [@@deriving sexp]

type common_session_data = {
  server_random          : Cstruct_sexp.t ; (* 32 bytes random from the server hello *)
  client_random          : Cstruct_sexp.t ; (* 32 bytes random from the client hello *)
  peer_certificate_chain : Cert.t list ;
  peer_certificate       : Cert.t option ;
  trust_anchor           : Cert.t option ;
  received_certificates  : Cert.t list ;
  own_certificate        : Cert.t list ;
  own_private_key        : Mirage_crypto_pk.Rsa.priv option ;
  own_name               : string option ;
  client_auth            : bool ;
  master_secret          : master_secret ;
  alpn_protocol          : string option ; (* selected alpn protocol after handshake *)
} [@@deriving sexp]

type session_data = {
  common_session_data    : common_session_data ;
  client_version         : tls_any_version ; (* version in client hello (needed in RSA client key exchange) *)
  ciphersuite            : Ciphersuite.ciphersuite ;
  group                  : group option ;
  renegotiation          : reneg_params ; (* renegotiation data *)
  session_id             : Cstruct_sexp.t ;
  extended_ms            : bool ;
} [@@deriving sexp]

(* state machine of the server *)
type server_handshake_state =
  | AwaitClientHello (* initial state *)
  | AwaitClientHelloRenegotiate
  | AwaitClientCertificate_RSA of session_data * hs_log
  | AwaitClientCertificate_DHE_RSA of session_data * dh_secret * hs_log
  | AwaitClientKeyExchange_RSA of session_data * hs_log (* server hello done is sent, and RSA key exchange used, waiting for a client key exchange message *)
  | AwaitClientKeyExchange_DHE_RSA of session_data * dh_secret * hs_log (* server hello done is sent, and DHE_RSA key exchange used, waiting for client key exchange *)
  | AwaitClientCertificateVerify of session_data * crypto_context * crypto_context * hs_log
  | AwaitClientChangeCipherSpec of session_data * crypto_context * crypto_context * hs_log (* client key exchange received, next should be change cipher spec *)
  | AwaitClientChangeCipherSpecResume of session_data * crypto_context * Cstruct_sexp.t * hs_log (* resumption: next should be change cipher spec *)
  | AwaitClientFinished of session_data * hs_log (* change cipher spec received, next should be the finished including a hmac over all handshake packets *)
  | AwaitClientFinishedResume of session_data * Cstruct_sexp.t * hs_log (* change cipher spec received, next should be the finished including a hmac over all handshake packets *)
  | Established (* handshake successfully completed *)
  [@@deriving sexp]

(* state machine of the client *)
type client_handshake_state =
  | ClientInitial (* initial state *)
  | AwaitServerHello of client_hello * (group * dh_secret) list * hs_log (* client hello is sent, handshake_params are half-filled *)
  | AwaitServerHelloRenegotiate of session_data * client_hello * hs_log (* client hello is sent, handshake_params are half-filled *)
  | AwaitCertificate_RSA of session_data * hs_log (* certificate expected with RSA key exchange *)
  | AwaitCertificate_DHE_RSA of session_data * hs_log (* certificate expected with DHE_RSA key exchange *)
  | AwaitServerKeyExchange_DHE_RSA of session_data * hs_log (* server key exchange expected with DHE_RSA *)
  | AwaitCertificateRequestOrServerHelloDone of session_data * Cstruct_sexp.t * Cstruct_sexp.t * hs_log (* server hello done expected, client key exchange and premastersecret are ready *)
  | AwaitServerHelloDone of session_data * signature_algorithm list option * Cstruct_sexp.t * Cstruct_sexp.t * hs_log (* server hello done expected, client key exchange and premastersecret are ready *)
  | AwaitServerChangeCipherSpec of session_data * crypto_context * Cstruct_sexp.t * hs_log (* change cipher spec expected *)
  | AwaitServerChangeCipherSpecResume of session_data * crypto_context * crypto_context * hs_log (* change cipher spec expected *)
  | AwaitServerFinished of session_data * Cstruct_sexp.t * hs_log (* finished expected with a hmac over all handshake packets *)
  | AwaitServerFinishedResume of session_data * hs_log (* finished expected with a hmac over all handshake packets *)
  | Established (* handshake successfully completed *)
  [@@deriving sexp]

type kdf = {
  secret : Cstruct_sexp.t ;
  cipher : Ciphersuite.ciphersuite13 ;
  hash : Ciphersuite.H.t ;
} [@@deriving sexp]

(* TODO needs log of CH..CF for post-handshake auth *)
(* TODO drop master_secret!? *)
type session_data13 = {
  common_session_data13  : common_session_data ;
  ciphersuite13          : Ciphersuite.ciphersuite13 ;
  master_secret          : kdf ;
  resumption_secret      : Cstruct_sexp.t ;
  state                  : epoch_state ;
  resumed                : bool ;
  client_app_secret      : Cstruct_sexp.t ;
  server_app_secret      : Cstruct_sexp.t ;
} [@@deriving sexp]

type client13_handshake_state =
  | AwaitServerHello13 of client_hello * (group * dh_secret) list * Cstruct_sexp.t (* this is for CH1 ~> HRR ~> CH2 <~ WAIT SH *)
  | AwaitServerEncryptedExtensions13 of session_data13 * Cstruct_sexp.t * Cstruct_sexp.t * Cstruct_sexp.t
  | AwaitServerCertificateRequestOrCertificate13 of session_data13 * Cstruct_sexp.t * Cstruct_sexp.t * Cstruct_sexp.t
  | AwaitServerCertificate13 of session_data13 * Cstruct_sexp.t * Cstruct_sexp.t * Cstruct_sexp.t
  | AwaitServerCertificateVerify13 of session_data13 * Cstruct_sexp.t * Cstruct_sexp.t * Cstruct_sexp.t
  | AwaitServerFinished13 of session_data13 * Cstruct_sexp.t * Cstruct_sexp.t * Cstruct_sexp.t
  | Established13
  [@@deriving sexp]

type server13_handshake_state =
  | AwaitClientHelloHRR13 (* if we sent out HRR (also to-be-used for tls13-only) *)
  | AwaitClientCertificate13 of session_data13 * Cstruct_sexp.t * crypto_context * session_ticket option * Cstruct_sexp.t
  | AwaitClientCertificateVerify13 of session_data13 * Cstruct_sexp.t * crypto_context * session_ticket option * Cstruct_sexp.t
  | AwaitClientFinished13 of Cstruct_sexp.t * crypto_context * session_ticket option * Cstruct_sexp.t
  | AwaitEndOfEarlyData13 of Cstruct_sexp.t * crypto_context * crypto_context * session_ticket option * Cstruct_sexp.t
  | Established13
  [@@deriving sexp]

type handshake_machina_state =
  | Client of client_handshake_state
  | Server of server_handshake_state
  | Client13 of client13_handshake_state
  | Server13 of server13_handshake_state
  [@@deriving sexp]

(* state during a handshake, used in the handlers *)
type handshake_state = {
  session          : [ `TLS of session_data | `TLS13 of session_data13 ] list ;
  protocol_version : tls_version ;
  early_data_left  : int32 ;
  machina          : handshake_machina_state ; (* state machine state *)
  config           : Config.config ; (* given config *)
  hs_fragment      : Cstruct_sexp.t ; (* handshake messages can be fragmented, leftover from before *)
} [@@deriving sexp]

(* connection state: initially None, after handshake a crypto context *)
type crypto_state = crypto_context option [@@deriving sexp]

(* record consisting of a content type and a byte vector *)
type record = Packet.content_type * Cstruct_sexp.t [@@deriving sexp]

(* response returned by a handler *)
type rec_resp = [
  | `Change_enc of crypto_context (* either instruction to change the encryptor to the given one *)
  | `Change_dec of crypto_context (* either change the decryptor to the given one *)
  | `Record     of record (* or a record which should be sent out *)
]

(* return type of handshake handlers *)
type handshake_return = handshake_state * rec_resp list

(* Top level state, encapsulating the entire session. *)
type state = {
  handshake : handshake_state ; (* the current handshake state *)
  decryptor : crypto_state ; (* the current decryption state *)
  encryptor : crypto_state ; (* the current encryption state *)
  fragment  : Cstruct_sexp.t ; (* the leftover fragment from TCP fragmentation *)
} [@@deriving sexp]

module V_err = struct
  type t = X509.Validation.validation_error
  let t_of_sexp _ = failwith "couldn't convert validatin error from sexp"
  let sexp_of_t v =
    let s = Fmt.to_to_string X509.Validation.pp_validation_error v in
    Sexplib.Sexp.Atom s
end

type error = [
  | `AuthenticationFailure of V_err.t
  | `NoConfiguredCiphersuite of Ciphersuite.ciphersuite list
  | `NoConfiguredVersions of tls_version list
  | `NoConfiguredSignatureAlgorithm of signature_algorithm list
  | `NoMatchingCertificateFound of string
  | `NoCertificateConfigured
  | `CouldntSelectCertificate
] [@@deriving sexp]

type client_hello_errors = [
  | `EmptyCiphersuites
  | `NotSetCiphersuites of Packet.any_ciphersuite list
  | `NoSupportedCiphersuite of Packet.any_ciphersuite list
  | `NotSetExtension of client_extension list
  | `HasSignatureAlgorithmsExtension
  | `NoSignatureAlgorithmsExtension
  | `NoGoodSignatureAlgorithms of signature_algorithm list
  | `NoKeyShareExtension
  | `NoSupportedGroupExtension
  | `NotSetSupportedGroup of Packet.named_group list
  | `NotSetKeyShare of (Packet.named_group * Cstruct_sexp.t) list
  | `NotSubsetKeyShareSupportedGroup of (Packet.named_group list * (Packet.named_group * Cstruct_sexp.t) list)
  | `Has0rttAfterHRR
  | `NoCookie
] [@@deriving sexp]

type fatal = [
  | `NoSecureRenegotiation
  | `NoSupportedGroup
  | `NoVersions of tls_any_version list
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
  | `BadRecordVersion of tls_any_version
  | `BadFinished
  | `HandshakeFragmentsNotEmpty
  | `InsufficientDH
  | `InvalidDH
  | `InvalidRenegotiation
  | `InvalidClientHello of client_hello_errors
  | `InvalidServerHello
  | `InvalidRenegotiationVersion of tls_version
  | `InappropriateFallback
  | `UnexpectedCCS
  | `UnexpectedHandshake of tls_handshake
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
] [@@deriving sexp]

type failure = [
  | `Error of error
  | `Fatal of fatal
] [@@deriving sexp]

(* Monadic control-flow core. *)
include Control.Or_error_make (struct type err = failure end)
type 'a eff = 'a t

let common_data_to_epoch common is_server peer_name =
  let own_random, peer_random =
    if is_server then
      common.server_random, common.client_random
    else
      common.client_random, common.server_random
  in
  let epoch : epoch_data =
    { state                  = `Established ;
      protocol_version       = `TLS_1_0 ;
      ciphersuite            = `DHE_RSA_WITH_AES_256_CBC_SHA ;
      peer_random ;
      peer_certificate       = common.peer_certificate ;
      peer_certificate_chain = common.peer_certificate_chain ;
      peer_name ;
      trust_anchor           = common.trust_anchor ;
      own_random ;
      own_certificate        = common.own_certificate ;
      own_private_key        = common.own_private_key ;
      own_name               = common.own_name ;
      received_certificates  = common.received_certificates ;
      master_secret          = common.master_secret ;
      alpn_protocol          = common.alpn_protocol ;
      session_id             = Cstruct.empty ;
      extended_ms            = false ;
    } in
  epoch

let epoch_of_session server peer_name protocol_version = function
  | `TLS (session : session_data) ->
    let epoch = common_data_to_epoch session.common_session_data server peer_name in
    {
      epoch with
      protocol_version       = protocol_version ;
      ciphersuite            = session.ciphersuite ;
      session_id             = session.session_id ;
      extended_ms            = session.extended_ms ;
    }
  | `TLS13 (session : session_data13) ->
    let epoch : epoch_data = common_data_to_epoch session.common_session_data13 server peer_name in
    {
      epoch with
      ciphersuite            = (session.ciphersuite13 :> Ciphersuite.ciphersuite) ;
      extended_ms            = true ; (* RFC 8446, Appendix D, last paragraph *)
      state                  = session.state ;
    }

let epoch_of_hs hs =
  let server =
    match hs.machina with
    | Client _ | Client13 _ -> false
    | Server _ | Server13 _ -> true
  and peer_name = Config.(hs.config.peer_name)
  in
  match hs.session with
  | []           -> None
  | session :: _ -> Some (epoch_of_session server peer_name hs.protocol_version session)
