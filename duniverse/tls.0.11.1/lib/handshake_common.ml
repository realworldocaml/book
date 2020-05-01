open Utils
open Core
open State

open Mirage_crypto

let empty = function [] -> true | _ -> false

let change_cipher_spec =
  (Packet.CHANGE_CIPHER_SPEC, Writer.assemble_change_cipher_spec)

let host_name_opt = function
  | None   -> None
  | Some x -> match Domain_name.of_string x with
    | Error _ -> None
    | Ok domain -> match Domain_name.host domain with
      | Error _ -> None
      | Ok host -> Some host

let hostname (h : client_hello) : [ `host ] Domain_name.t option =
  host_name_opt
    (map_find ~f:(function `Hostname s -> Some s | _ -> None) h.extensions)

let get_secure_renegotiation exts =
  map_find
    exts
    ~f:(function `SecureRenegotiation data -> Some data | _ -> None)

let get_alpn_protocols (ch : client_hello) =
  map_find ~f:(function `ALPN protocols -> Some protocols | _ -> None) ch.extensions

let get_alpn_protocol (sh : server_hello) =
  map_find ~f:(function `ALPN protocol -> Some protocol | _ -> None) sh.extensions

let empty_session = {
  server_random          = Cstruct.create 0 ;
  client_random          = Cstruct.create 0 ;
  client_version         = Supported TLS_1_2 ;
  ciphersuite            = `TLS_DHE_RSA_WITH_AES_256_CBC_SHA ;
  peer_certificate_chain = [] ;
  peer_certificate       = None ;
  trust_anchor           = None ;
  received_certificates  = [] ;
  own_certificate        = [] ;
  own_private_key        = None ;
  own_name               = None ;
  master_secret          = Cstruct.create 0 ;
  renegotiation          = Cstruct.(create 0, create 0) ;
  client_auth            = false ;
  session_id             = Cstruct.create 0 ;
  extended_ms            = false ;
  alpn_protocol          = None ;
}

let session_of_epoch (epoch : epoch_data) : session_data = {
  empty_session with
  ciphersuite = epoch.ciphersuite ;
  peer_certificate = epoch.peer_certificate ;
  trust_anchor = epoch.trust_anchor ;
  own_certificate = epoch.own_certificate ;
  own_private_key = epoch.own_private_key ;
  received_certificates = epoch.received_certificates ;
  peer_certificate_chain = epoch.peer_certificate_chain ;
  master_secret = epoch.master_secret ;
  own_name = epoch.own_name ;
  session_id = epoch.session_id ;
  extended_ms = epoch.extended_ms ;
  alpn_protocol = epoch.alpn_protocol ;
}

let supported_protocol_version (min, max) v =
  match version_ge v min, version_ge v max with
    | _   , true -> Some max
    | true, _    -> any_version_to_version v
    | _   , _    -> None

let to_client_ext_type = function
  | `Hostname _            -> `Hostname
  | `MaxFragmentLength _   -> `MaxFragmentLength
  | `EllipticCurves _      -> `EllipticCurves
  | `ECPointFormats _      -> `ECPointFormats
  | `SecureRenegotiation _ -> `SecureRenegotiation
  | `Padding _             -> `Padding
  | `SignatureAlgorithms _ -> `SignatureAlgorithms
  | `UnknownExtension _    -> `UnknownExtension
  | `ExtendedMasterSecret  -> `ExtendedMasterSecret
  | `ALPN _                -> `ALPN

let to_server_ext_type = function
  | `Hostname              -> `Hostname
  | `MaxFragmentLength _   -> `MaxFragmentLength
  | `ECPointFormats _      -> `ECPointFormats
  | `SecureRenegotiation _ -> `SecureRenegotiation
  | `UnknownExtension _    -> `UnknownExtension
  | `ExtendedMasterSecret  -> `ExtendedMasterSecret
  | `ALPN _                -> `ALPN

let extension_types t exts = List.(
  exts |> map t
       |> filter @@ function `UnknownExtension -> false | _ -> true
  )

(* a server hello may only contain extensions which are also in the client hello *)
(*  RFC5246, 7.4.7.1
   An extension type MUST NOT appear in the ServerHello unless the same
   extension type appeared in the corresponding ClientHello.  If a
   client receives an extension type in ServerHello that it did not
   request in the associated ClientHello, it MUST abort the handshake
   with an unsupported_extension fatal alert. *)
let server_exts_subset_of_client sexts cexts =
  let (sexts', cexts') =
    (extension_types to_server_ext_type sexts, extension_types to_client_ext_type cexts) in
  List_set.subset sexts' cexts'

let client_hello_valid ch =
  (* let open Ciphersuite in
  match ch.version with
    | TLS_1_0 ->
       if List.mem TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA ch.ciphersuites then
         return ()
       else
         fail HANDSHAKE_FAILURE
    | TLS_1_1 ->
       if List.mem TLS_RSA_WITH_3DES_EDE_CBC_SHA ch.ciphersuites then
         return ()
       else
         fail HANDSHAKE_FAILURE
    | TLS_1_2 ->
       if List.mem TLS_RSA_WITH_AES_128_CBC_SHA ch.ciphersuites then
         return ()
       else
         fail HANDSHAKE_FAILURE *)

  not (empty ch.ciphersuites)
  &&

  (* android 4.4 and davdroid do not send proper sets!
  (List_set.is_proper_set ch.ciphersuites)
  &&
  *)

  (* TODO: if ecc ciphersuite, require ellipticcurves and ecpointformats extensions! *)
  List_set.is_proper_set (extension_types to_client_ext_type ch.extensions)
  &&

  ( match ch.client_version with
    | Supported TLS_1_2 | TLS_1_X _                  -> true
    | SSL_3 | Supported TLS_1_0 | Supported TLS_1_1  ->
        let has_sig_algo =
          List.exists (function `SignatureAlgorithms _ -> true | _ -> false)
            ch.extensions in
        not has_sig_algo )

let server_hello_valid sh =
  List_set.is_proper_set (extension_types to_server_ext_type sh.extensions)
  (* TODO:
      - EC stuff must be present if EC ciphersuite chosen
   *)

let (<+>) = Cs.(<+>)

let signature version data sig_algs hashes private_key =
  match version with
  | TLS_1_0 | TLS_1_1 ->
    let data = Hash.MD5.digest data <+> Hash.SHA1.digest data in
    let signed = Mirage_crypto_pk.Rsa.PKCS1.sig_encode ~key:private_key data in
    return (Writer.assemble_digitally_signed signed)
  | TLS_1_2 ->
    (* if no signature_algorithms extension is sent by the client,
       support for md5 and sha1 can be safely assumed! *)
    ( match sig_algs with
      | None              -> return `SHA1
      | Some client_algos ->
        let client_hashes =
          List.(map fst @@ filter (fun (_, x) -> x = Packet.RSA) client_algos)
        in
        match first_match client_hashes hashes with
        | None      -> fail (`Error (`NoConfiguredHash client_hashes))
        | Some hash -> return hash ) >|= fun hash_algo ->
    let hash = Hash.digest hash_algo data in
    let cs = X509.Certificate.encode_pkcs1_digest_info (hash_algo, hash) in
    let sign = Mirage_crypto_pk.Rsa.PKCS1.sig_encode ~key:private_key cs in
    Writer.assemble_digitally_signed_1_2 hash_algo Packet.RSA sign

let peer_rsa_key = function
  | None -> fail (`Fatal `NoCertificateReceived)
  | Some cert ->
    match X509.Certificate.public_key cert with
    | `RSA key -> return key
    | _        -> fail (`Fatal `NotRSACertificate)

let verify_digitally_signed version hashes data signature_data certificate =
  let signature_verifier version data =
    let open Reader in
    match version with
    | TLS_1_0 | TLS_1_1 ->
      ( match parse_digitally_signed data with
        | Ok signature ->
          let compare_hashes should data =
            let computed_sig = Hash.MD5.digest data <+> Hash.SHA1.digest data in
            guard (Cs.equal should computed_sig) (`Fatal `RSASignatureMismatch)
          in
          return (signature, compare_hashes)
        | Error re -> fail (`Fatal (`ReaderError re)) )
    | TLS_1_2 ->
      ( match parse_digitally_signed_1_2 data with
        | Ok (hash_algo, Packet.RSA, signature) ->
          guard (List.mem hash_algo hashes) (`Error (`NoConfiguredHash hashes)) >>= fun () ->
          let compare_hashes should data =
            match X509.Certificate.decode_pkcs1_digest_info should with
            | Ok (hash_algo', target) when hash_algo = hash_algo' ->
              guard (Crypto.digest_eq hash_algo ~target data) (`Fatal `RSASignatureMismatch)
            | _ -> fail (`Fatal `HashAlgorithmMismatch)
          in
          return (signature, compare_hashes)
        | Ok _ -> fail (`Fatal `NotRSASignature)
        | Error re -> fail (`Fatal (`ReaderError re)) )

  and signature pubkey raw_signature =
    match Mirage_crypto_pk.Rsa.PKCS1.sig_decode ~key:pubkey raw_signature with
    | Some signature -> return signature
    | None -> fail (`Fatal `RSASignatureVerificationFailed)
  in

  signature_verifier version data >>= fun (raw_signature, verifier) ->
  peer_rsa_key certificate >>= fun pubkey ->
  signature pubkey raw_signature >>= fun signature ->
  verifier signature signature_data

let validate_chain authenticator certificates hostname =
  let authenticate authenticator host certificates =
    match authenticator ~host certificates with
    | Error err  -> fail (`Error (`AuthenticationFailure err))
    | Ok anchor -> return anchor

  and key_size min cs =
    let check c =
      match X509.Certificate.public_key c with
      | `RSA key when Mirage_crypto_pk.Rsa.pub_bits key >= min -> true
      | _                                                      -> false
    in
    guard (List.for_all check cs) (`Fatal `KeyTooSmall)

  and parse_certificates certs =
    let certificates =
      let f cs = match X509.Certificate.decode_der cs with Ok c -> Some c | _ -> None in
      filter_map ~f certs
    in
    guard (List.length certs = List.length certificates) (`Fatal `BadCertificateChain) >|= fun () ->
    certificates

  in

  (* RFC5246: must be x509v3, take signaturealgorithms into account! *)
  (* RFC2246/4346: is generally x509v3, signing algorithm for certificate _must_ be same as algorithm for certificate key *)
  parse_certificates certificates >>= fun certs ->
  let server = match certs with
    | s::_ -> Some s
    | [] -> None
  in
  match authenticator with
  | None -> return (server, certs, [], None)
  | Some authenticator ->
    authenticate authenticator hostname certs >>= fun anchor ->
    key_size Config.min_rsa_key_size certs >|= fun () ->
    Utils.option
      (server, certs, [], None)
      (fun (chain, anchor) -> (server, certs, chain, Some anchor))
      anchor
