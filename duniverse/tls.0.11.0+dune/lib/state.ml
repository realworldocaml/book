(* Defines all high-level datatypes for the TLS library. It is opaque to clients
 of this library, and only used from within the library. *)

open Sexplib
open Sexplib.Conv

open Core
open Nocrypto

type hmac_key = Cstruct.t

type 'k stream_state = {
  cipher         : (module Cipher_stream.S with type key = 'k) ;
  cipher_secret  : 'k ;
  hmac           : Hash.hash ;
  hmac_secret    : hmac_key
}

(* initialisation vector style, depending on TLS version *)
type iv_mode =
  | Iv of Cstruct.t  (* traditional CBC (reusing last cipherblock) *)
  | Random_iv        (* TLS 1.1 and higher explicit IV (we use random) *)

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
  | Stream : 'k stream_state -> cipher_st
  | CBC    : 'k cbc_state -> cipher_st
  | AEAD   : 'k aead_state -> cipher_st

(* Sexplib stubs -- rethink how to play with crypto. *)
let sexp_of_cipher_st = function
  | Stream _ -> Sexp.Atom "<stream-state>"
  | CBC _    -> Sexp.Atom "<cbc-state>"
  | AEAD _   -> Sexp.Atom "<aead-state>"

let cipher_st_of_sexp =
  Conv.of_sexp_error "cipher_st_of_sexp: not implemented"
(* *** *)

(* context of a TLS connection (both in and out has each one of these) *)
type crypto_context = {
  sequence  : int64 ; (* sequence number *)
  cipher_st : cipher_st ; (* cipher state *)
}

(* the raw handshake log we need to carry around *)
type hs_log = Cstruct.t list
(* diffie hellman group and secret *)
type dh_sent = Dh.group * Dh.secret

(* a collection of client and server verify bytes for renegotiation *)
type reneg_params = Cstruct.t * Cstruct.t

let session_data_of_sexp _ = assert false
let sexp_of_session_data _ = assert false

type session_data = {
  server_random          : Cstruct.t ; (* 32 bytes random from the server hello *)
  client_random          : Cstruct.t ; (* 32 bytes random from the client hello *)
  client_version         : tls_any_version ; (* version in client hello (needed in RSA client key exchange) *)
  ciphersuite            : Ciphersuite.ciphersuite ;
  peer_certificate_chain : X509.Certificate.t list ;
  peer_certificate       : X509.Certificate.t option ;
  trust_anchor           : X509.Certificate.t option ;
  received_certificates  : X509.Certificate.t list ;
  own_certificate        : X509.Certificate.t list ;
  own_private_key        : Nocrypto.Rsa.priv option ;
  master_secret          : master_secret ;
  renegotiation          : reneg_params ; (* renegotiation data *)
  own_name               : [`host] Domain_name.t option ;
  client_auth            : bool ;
  session_id             : Cstruct.t ;
  extended_ms            : bool ;
  alpn_protocol          : string option ; (* selected alpn protocol after handshake *)
}

(* state machine of the server *)
type server_handshake_state =
  | AwaitClientHello (* initial state *)
  | AwaitClientHelloRenegotiate
  | AwaitClientCertificate_RSA of session_data * hs_log
  | AwaitClientCertificate_DHE_RSA of session_data * dh_sent * hs_log
  | AwaitClientKeyExchange_RSA of session_data * hs_log (* server hello done is sent, and RSA key exchange used, waiting for a client key exchange message *)
  | AwaitClientKeyExchange_DHE_RSA of session_data * dh_sent * hs_log (* server hello done is sent, and DHE_RSA key exchange used, waiting for client key exchange *)
  | AwaitClientCertificateVerify of session_data * crypto_context * crypto_context * hs_log
  | AwaitClientChangeCipherSpec of session_data * crypto_context * crypto_context * hs_log (* client key exchange received, next should be change cipher spec *)
  | AwaitClientChangeCipherSpecResume of session_data * crypto_context * Cstruct.t * hs_log (* resumption: next should be change cipher spec *)
  | AwaitClientFinished of session_data * hs_log (* change cipher spec received, next should be the finished including a hmac over all handshake packets *)
  | AwaitClientFinishedResume of session_data * Cstruct.t * hs_log (* change cipher spec received, next should be the finished including a hmac over all handshake packets *)
  | Established (* handshake successfully completed *)

(* state machine of the client *)
type client_handshake_state =
  | ClientInitial (* initial state *)
  | AwaitServerHello of client_hello * hs_log (* client hello is sent, handshake_params are half-filled *)
  | AwaitServerHelloRenegotiate of session_data * client_hello * hs_log (* client hello is sent, handshake_params are half-filled *)
  | AwaitCertificate_RSA of session_data * hs_log (* certificate expected with RSA key exchange *)
  | AwaitCertificate_DHE_RSA of session_data * hs_log (* certificate expected with DHE_RSA key exchange *)
  | AwaitServerKeyExchange_DHE_RSA of session_data * hs_log (* server key exchange expected with DHE_RSA *)
  | AwaitCertificateRequestOrServerHelloDone of session_data * Cstruct.t * Cstruct.t * hs_log (* server hello done expected, client key exchange and premastersecret are ready *)
  | AwaitServerHelloDone of session_data * (Hash.hash * Packet.signature_algorithm_type) list option * Cstruct.t * Cstruct.t * hs_log (* server hello done expected, client key exchange and premastersecret are ready *)
  | AwaitServerChangeCipherSpec of session_data * crypto_context * Cstruct.t * hs_log (* change cipher spec expected *)
  | AwaitServerChangeCipherSpecResume of session_data * crypto_context * crypto_context * hs_log (* change cipher spec expected *)
  | AwaitServerFinished of session_data * Cstruct.t * hs_log (* finished expected with a hmac over all handshake packets *)
  | AwaitServerFinishedResume of session_data * hs_log (* finished expected with a hmac over all handshake packets *)
  | Established (* handshake successfully completed *)

type handshake_machina_state =
  | Client of client_handshake_state
  | Server of server_handshake_state

(* state during a handshake, used in the handlers *)
type handshake_state = {
  session          : session_data list ;
  protocol_version : tls_version ;
  machina          : handshake_machina_state ; (* state machine state *)
  config           : Config.config ; (* given config *)
  hs_fragment      : Cstruct.t ; (* handshake messages can be fragmented, leftover from before *)
}

(* connection state: initially None, after handshake a crypto context *)
type crypto_state = crypto_context option

(* record consisting of a content type and a byte vector *)
type record = Packet.content_type * Cstruct.t

(* response returned by a handler *)
type rec_resp = [
  | `Change_enc of crypto_state (* either instruction to change the encryptor to the given one *)
  | `Change_dec of crypto_state (* either change the decryptor to the given one *)
  | `Record     of record (* or a record which should be sent out *)
]

(* return type of handshake handlers *)
type handshake_return = handshake_state * rec_resp list

(* Top level state, encapsulating the entire session. *)
type state = {
  handshake : handshake_state ; (* the current handshake state *)
  decryptor : crypto_state ; (* the current decryption state *)
  encryptor : crypto_state ; (* the current encryption state *)
  fragment  : Cstruct.t ; (* the leftover fragment from TCP fragmentation *)
}

type error = [
  | `AuthenticationFailure of X509.Validation.validation_error
  | `NoConfiguredCiphersuite of Ciphersuite.ciphersuite list
  | `NoConfiguredVersion of tls_version
  | `NoConfiguredHash of Hash.hash list
  | `NoMatchingCertificateFound of [`host] Domain_name.t
  | `NoCertificateConfigured
  | `CouldntSelectCertificate
]

type fatal = [
  | `NoSecureRenegotiation
  | `NoCiphersuite of Packet.any_ciphersuite list
  | `NoVersion of tls_any_version
  | `ReaderError of Reader.error
  | `NoCertificateReceived
  | `NotRSACertificate
  | `NotRSASignature
  | `KeyTooSmall
  | `RSASignatureMismatch
  | `RSASignatureVerificationFailed
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
  | `InvalidDH
  | `InvalidRenegotiation
  | `InvalidClientHello
  | `InvalidServerHello
  | `InvalidRenegotiationVersion of tls_version
  | `InappropriateFallback
  | `UnexpectedCCS
  | `UnexpectedHandshake of tls_handshake
  | `InvalidCertificateUsage
  | `InvalidCertificateExtendedUsage
  | `InvalidSession
  | `NoApplicationProtocol
]

let failure_of_sexp _ = failwith "not supported"
let sexp_of_failure _ = Sexplib.Sexp.Atom "Failure"

type failure = [
  | `Error of error
  | `Fatal of fatal
]

(* Monadic control-flow core. *)
include Control.Or_error_make (struct type err = failure end)
type 'a eff = 'a t
