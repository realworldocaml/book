open Utils
open Core
open State

open Mirage_crypto

let src = Logs.Src.create "handshake" ~doc:"TLS handshake"
module Log = (val Logs.src_log src : Logs.LOG)

let trace_cipher cipher =
  let kex, papr = Ciphersuite.get_kex_privprot cipher in
  let sexp = lazy (Sexplib.Sexp.(List Ciphersuite.(
      [ sexp_of_key_exchange_algorithm kex ;
        sexp_of_payload_protection papr ])))
  in
  Tracing.sexp ~tag:"cipher" sexp

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

let groups (h : client_hello) =
  match map_find ~f:(function `SupportedGroups g -> Some g | _ -> None) h.extensions with
  | Some xs ->
    List.fold_left (fun acc g ->
        match named_group_to_group g with Some g -> g :: acc | _ -> acc)
      [] xs
  | None -> []

let rec find_matching host certs =
  match certs with
  | (s::_, _) as chain ::xs ->
    if X509.Certificate.supports_hostname s host then
      Some chain
    else
      find_matching host xs
  | _::xs -> find_matching host xs (* this should never happen! *)
  | [] -> None

let agreed_cert certs hostname =
  let match_host ?default host certs =
     match find_matching host certs with
     | Some x -> return x
     | None   -> match default with
       | Some c -> return c
       | None   -> fail (`Error (`NoMatchingCertificateFound (Domain_name.to_string host)))
  in
  match certs, hostname with
  | `None                    , _      -> fail (`Error `NoCertificateConfigured)
  | `Single c                , _      -> return c
  | `Multiple_default (c, _) , None   -> return c
  | `Multiple cs             , Some h -> match_host h cs
  | `Multiple_default (c, cs), Some h -> match_host h cs ~default:c
  | _                                 -> fail (`Error `CouldntSelectCertificate)

let get_secure_renegotiation exts =
  map_find
    exts
    ~f:(function `SecureRenegotiation data -> Some data | _ -> None)

let get_alpn_protocols (ch : client_hello) =
  map_find ~f:(function `ALPN protocols -> Some protocols | _ -> None) ch.extensions

let alpn_protocol config ch =
  match config.Config.alpn_protocols, get_alpn_protocols ch with
  | _, None | [], _ -> return None
  | configured, Some client -> match first_match client configured with
    | Some proto -> return (Some proto)
    | None ->
      (* RFC7301 Section 3.2:
         In the event that the server supports no protocols that the client
         advertises, then the server SHALL respond with a fatal
         "no_application_protocol" alert. *)
      fail (`Fatal `NoApplicationProtocol)

let get_alpn_protocol (sh : server_hello) =
  map_find ~f:(function `ALPN protocol -> Some protocol | _ -> None) sh.extensions

let empty_common_session_data = {
  server_random          = Cstruct.create 0 ;
  client_random          = Cstruct.create 0 ;
  peer_certificate_chain = [] ;
  peer_certificate       = None ;
  trust_anchor           = None ;
  received_certificates  = [] ;
  own_certificate        = [] ;
  own_private_key        = None ;
  own_name               = None ;
  client_auth            = false ;
  master_secret          = Cstruct.empty ;
  alpn_protocol          = None ;
}

let empty_session = {
  common_session_data = empty_common_session_data ;
  client_version      = `TLS_1_2 ;
  ciphersuite         = `DHE_RSA_WITH_AES_256_CBC_SHA ;
  group               = Some `FFDHE2048 ;
  renegotiation       = Cstruct.(empty, empty) ;
  session_id          = Cstruct.empty ;
  extended_ms         = false ;
}

let empty_session13 cipher = {
  common_session_data13 = empty_common_session_data ;
  ciphersuite13         = cipher ;
  master_secret         = Handshake_crypto13.empty cipher ;
  resumption_secret     = Cstruct.empty ;
  state                 = `Established ;
  resumed               = false ;
  client_app_secret     = Cstruct.empty ;
  server_app_secret     = Cstruct.empty ;
}

let common_session_data_of_epoch (epoch : epoch_data) common_session_data =
  {
    common_session_data with
    peer_certificate = epoch.peer_certificate ;
    trust_anchor = epoch.trust_anchor ;
    own_certificate = epoch.own_certificate ;
    own_private_key = epoch.own_private_key ;
    received_certificates = epoch.received_certificates ;
    peer_certificate_chain = epoch.peer_certificate_chain ;
    master_secret = epoch.master_secret ;
    own_name = epoch.own_name ;
    alpn_protocol = epoch.alpn_protocol ;
  }

let session_of_epoch (epoch : epoch_data) : session_data =
  let empty = empty_session in
  let common_session_data = common_session_data_of_epoch epoch empty.common_session_data in
  { empty with
    common_session_data ;
    ciphersuite = epoch.ciphersuite ;
    session_id = epoch.session_id ;
    extended_ms = epoch.extended_ms ;
  }

let session13_of_epoch cipher (epoch : epoch_data) : session_data13 =
  let empty = empty_session13 cipher in
  let common_session_data13 = common_session_data_of_epoch epoch empty.common_session_data13 in
  { empty with
    common_session_data13 ;
    ciphersuite13 = cipher ;
    state = epoch.state ;
  }

let supported_protocol_version (min, max) v =
  if compare_tls_version min v > 0 then
    None
  else if compare_tls_version v max > 0 then
    None
  else
    Some v

let to_client_ext_type = function
  | `Hostname _            -> `Hostname
  | `MaxFragmentLength _   -> `MaxFragmentLength
  | `SupportedGroups _     -> `SupportedGroups
  | `ECPointFormats _      -> `ECPointFormats
  | `SecureRenegotiation _ -> `SecureRenegotiation
  | `Padding _             -> `Padding
  | `SignatureAlgorithms _ -> `SignatureAlgorithms
  | `UnknownExtension _    -> `UnknownExtension
  | `ExtendedMasterSecret  -> `ExtendedMasterSecret
  | `ALPN _                -> `ALPN
  | `KeyShare _            -> `KeyShare
  | `EarlyDataIndication   -> `EarlyDataIndication
  | `PreSharedKeys _       -> `PreSharedKey
  | `Draft _               -> `Draft
  | `SupportedVersions _   -> `SupportedVersion
  | `PostHandshakeAuthentication -> `PostHandshakeAuthentication
  | `Cookie _              -> `Cookie
  | `PskKeyExchangeModes _ -> `PskKeyExchangeMode

let to_server_ext_type = function
  | `Hostname              -> `Hostname
  | `MaxFragmentLength _   -> `MaxFragmentLength
  | `ECPointFormats _      -> `ECPointFormats
  | `SecureRenegotiation _ -> `SecureRenegotiation
  | `UnknownExtension _    -> `UnknownExtension
  | `ExtendedMasterSecret  -> `ExtendedMasterSecret
  | `ALPN _                -> `ALPN
  | `KeyShare _            -> `KeyShare
  | `EarlyDataIndication   -> `EarlyDataIndication
  | `PreSharedKey _        -> `PreSharedKey
  | `Draft _               -> `Draft
  | `SelectedVersion _     -> `SupportedVersion

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
  List_set.subset sexts' (`Cookie :: cexts')

module Group = struct
  type t = Packet.named_group
  let compare = Stdlib.compare
end

module GroupSet = Set.Make(Group)

(* Set.of_list appeared only in 4.02, for 4.01 compatibility *)
let of_list xs = List.fold_right GroupSet.add xs GroupSet.empty

let client_hello_valid version (ch : client_hello) =
  (* match ch.version with
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
  let sig_alg =
    map_find
      ~f:(function `SignatureAlgorithms sa -> Some sa | _ -> None)
      ch.extensions
  and key_share =
    map_find
      ~f:(function `KeyShare ks -> Some ks | _ -> None)
      ch.extensions
  and groups =
    map_find
      ~f:(function `SupportedGroups gs -> Some gs | _ -> None)
      ch.extensions
  in

  let version_good = match version with
    | `TLS_1_2 | `TLS_1_X _ -> `Ok
    | `TLS_1_3 ->
      ( let good_sig_alg =
          List.exists (fun sa -> List.mem sa Config.supported_signature_algorithms)
        in
        match sig_alg with
        | None -> `Error `NoSignatureAlgorithmsExtension
        | Some sig_alg when good_sig_alg sig_alg ->
          ( match key_share, groups with
            | None, _ -> `Error `NoKeyShareExtension
            | _, None -> `Error `NoSupportedGroupExtension
            | Some ks, Some gs ->
              match
                List_set.is_proper_set gs,
                List_set.is_proper_set (List.map fst ks),
                GroupSet.subset (of_list (List.map fst ks)) (of_list gs)
              with
              | true, true, true -> `Ok
              | false, _, _ -> `Error (`NotSetSupportedGroup gs)
              | _, false, _ -> `Error (`NotSetKeyShare ks)
              | _, _, false -> `Error (`NotSubsetKeyShareSupportedGroup (gs, ks)) )
        | Some x -> `Error (`NoGoodSignatureAlgorithms x)
      )
    | `SSL_3 | `TLS_1_0 | `TLS_1_1 ->
      Utils.option `Ok (fun _ -> `Error `HasSignatureAlgorithmsExtension) sig_alg
  in

  let share_ciphers =
    match
      first_match (filter_map ~f:Ciphersuite.any_ciphersuite_to_ciphersuite ch.ciphersuites) Config.Ciphers.supported
    with
    | None -> false
    | Some _ -> true
  in
  match
    not (empty ch.ciphersuites),
    List_set.is_proper_set ch.ciphersuites,
    share_ciphers,
    List_set.is_proper_set (extension_types to_client_ext_type ch.extensions)
  with
  | true, _, true, true -> version_good
  | false, _ , _, _ -> `Error `EmptyCiphersuites
  (*  | _, false, _, _ -> `Error (`NotSetCiphersuites ch.ciphersuites) *)
  | _, _, false, _ -> `Error (`NoSupportedCiphersuite ch.ciphersuites)
  | _, _, _, false -> `Error (`NotSetExtension ch.extensions)


let server_hello_valid (sh : server_hello) =
  (* let open Ciphersuite in *)
  List_set.is_proper_set (extension_types to_server_ext_type sh.extensions)
  (* TODO:
      - EC stuff must be present if EC ciphersuite chosen
   *)

let (<+>) = Cs.(<+>)

let to_sign_1_3 context_string =
  (* input is prepended by 64 * 0x20 (to avoid cross-version attacks) *)
  (* input for signature now contains also a context string *)
  let prefix = Cstruct.create 64 in
  Cstruct.memset prefix 0x20 ;
  let ctx =
    let stop = Cstruct.create 1 (* trailing 0 byte *) in
    match context_string with
    | None -> stop
    | Some x -> Cstruct.of_string x <+> stop
  in
  prefix <+> ctx

let signature version ?context_string data client_sig_algs signature_algorithms private_key =
  try
    begin match version with
  | `TLS_1_0 | `TLS_1_1 ->
    let data = Hash.MD5.digest data <+> Hash.SHA1.digest data in
    let signed = Mirage_crypto_pk.Rsa.PKCS1.sig_encode ~key:private_key data in
    return (Writer.assemble_digitally_signed signed)
  | `TLS_1_2 ->
    (* if no signature_algorithms extension is sent by the client,
       support for md5 and sha1 can be safely assumed! *)
    ( match client_sig_algs with
      | None              -> return `RSA_PKCS1_SHA1
      | Some client_algos ->
        match first_match client_algos signature_algorithms with
        | None      -> fail (`Error (`NoConfiguredSignatureAlgorithm client_algos))
        | Some sig_alg -> return sig_alg ) >|= fun sig_alg ->
    let hash_alg = Core.hash_of_signature_algorithm sig_alg in
    begin match signature_scheme_of_signature_algorithm sig_alg with
      | `PSS ->
        let module H = (val (Hash.module_of hash_alg)) in
        let module PSS = Mirage_crypto_pk.Rsa.PSS(H) in
        let sign = PSS.sign ~key:private_key (`Message data) in
        Writer.assemble_digitally_signed_1_2 sig_alg sign
      | `PKCS1 ->
        let hash = Hash.digest hash_alg data in
        let cs = X509.Certificate.encode_pkcs1_digest_info (hash_alg, hash) in
        let sign = Mirage_crypto_pk.Rsa.PKCS1.sig_encode ~key:private_key cs in
        Writer.assemble_digitally_signed_1_2 sig_alg sign
    end
  | `TLS_1_3 ->
    (* RSA-PSS is used *)
    let prefix = to_sign_1_3 context_string in
    ( match client_sig_algs with
      | None              -> return `RSA_PSS_RSAENC_SHA256
      | Some client_algos ->
        (* SHA1 must not be used - all our PSS_RSAENC only use sha2 *)
        match first_match client_algos signature_algorithms with
        | None -> fail (`Error (`NoConfiguredSignatureAlgorithm client_algos))
        | Some sig_alg -> return sig_alg ) >>= fun sig_alg ->
    let hash_algo = hash_of_signature_algorithm sig_alg in
    match signature_scheme_of_signature_algorithm sig_alg with
    | `PSS ->
      let module H = (val (Hash.module_of hash_algo)) in
      let module PSS = Mirage_crypto_pk.Rsa.PSS(H) in
      let to_sign = prefix <+> data in
      let signature = PSS.sign ~key:private_key (`Message to_sign) in
      return (Writer.assemble_digitally_signed_1_2 sig_alg signature)
    | _ -> fail (`Error (`NoConfiguredSignatureAlgorithm [])) (*TODO different warning, types *)
    end
  with Mirage_crypto_pk.Rsa.Insufficient_key ->
    fail (`Fatal `KeyTooSmall)

let peer_rsa_key = function
  | None -> fail (`Fatal `NoCertificateReceived)
  | Some cert ->
    match X509.Certificate.public_key cert with
    | `RSA key -> return key
    | _        -> fail (`Fatal `NotRSACertificate)

let verify_digitally_signed version ?context_string sig_algs data signature_data certificate =
  peer_rsa_key certificate >>= fun pubkey ->

  let decode_pkcs1_signature raw_signature =
    match Mirage_crypto_pk.Rsa.PKCS1.sig_decode ~key:pubkey raw_signature with
    | Some signature -> return signature
    | None -> fail (`Fatal `RSASignatureVerificationFailed)
  in

  match version with
  | `TLS_1_0 | `TLS_1_1 ->
    ( match Reader.parse_digitally_signed data with
      | Ok signature ->
         let compare_hashes should data =
           let computed_sig = Hash.MD5.digest data <+> Hash.SHA1.digest data in
           guard (Cs.equal should computed_sig) (`Fatal `RSASignatureMismatch)
         in
         decode_pkcs1_signature signature >>= fun raw ->
         compare_hashes raw signature_data
      | Error re -> fail (`Fatal (`ReaderError re)) )
  | `TLS_1_2 ->
     ( match Reader.parse_digitally_signed_1_2 data with
       | Ok (sig_alg, signature) ->
         guard (List.mem sig_alg sig_algs) (`Error (`NoConfiguredSignatureAlgorithm sig_algs)) >>= fun () ->
         let hash_algo = hash_of_signature_algorithm sig_alg in
         begin match signature_scheme_of_signature_algorithm sig_alg with
           | `PSS ->
             let module H = (val (Hash.module_of hash_algo)) in
             let module PSS = Mirage_crypto_pk.Rsa.PSS(H) in
             guard (PSS.verify ~key:pubkey ~signature (`Message signature_data))
               (`Fatal `RSASignatureMismatch)
           | `PKCS1 ->
             let compare_hashes should data =
               match X509.Certificate.decode_pkcs1_digest_info should with
               | Ok (hash_algo', target) when hash_algo = hash_algo' ->
                 guard (Crypto.digest_eq hash_algo ~target data) (`Fatal `RSASignatureMismatch)
               | _ -> fail (`Fatal `HashAlgorithmMismatch)
             in
             decode_pkcs1_signature signature >>= fun raw ->
             compare_hashes raw signature_data
         end
       | Error re -> fail (`Fatal (`ReaderError re)) )
  | `TLS_1_3 ->
    ( match Reader.parse_digitally_signed_1_2 data with
      | Ok (sig_alg, signature) ->
        guard (List.mem sig_alg sig_algs) (`Error (`NoConfiguredSignatureAlgorithm sig_algs)) >>= fun () ->
        let hash_algo = hash_of_signature_algorithm sig_alg in
        begin match signature_scheme_of_signature_algorithm sig_alg with
          | `PSS ->
            let module H = (val (Hash.module_of hash_algo)) in
            let module PSS = Mirage_crypto_pk.Rsa.PSS(H) in
            let data =
              let prefix = to_sign_1_3 context_string in
              prefix <+> signature_data
            in
            guard (PSS.verify ~key:pubkey ~signature (`Message data))
              (`Fatal `RSASignatureMismatch)
          | `PKCS1 ->
            fail (`Fatal `UnsupportedSignatureScheme)
        end
      | Error re -> fail (`Fatal (`ReaderError re)))

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

let output_key_update ~request state =
  let hs = state.handshake in
  match hs.session with
  | `TLS13 session :: _ ->
    begin match hs.machina with
      | Client13 Established13 ->
        let client_app_secret, client_ctx =
          Handshake_crypto13.app_secret_n_1
            session.master_secret session.client_app_secret
        in
        Ok ({ session with client_app_secret }, client_ctx)
      | Server13 Established13 ->
        let server_app_secret, server_ctx =
          Handshake_crypto13.app_secret_n_1
            session.master_secret session.server_app_secret
        in
        Ok ({ session with server_app_secret }, server_ctx)
      | _ -> Error (`Fatal `InvalidSession)
    end >|= fun (session', encryptor) ->
    let handshake = { hs with session = `TLS13 session' :: hs.session } in
    let ku =
      let p =
        Packet.(if request then UPDATE_REQUESTED else UPDATE_NOT_REQUESTED)
      in
      KeyUpdate p
    in
    let out = Writer.assemble_handshake ku in
    { state with encryptor = Some encryptor ; handshake },
    (Packet.HANDSHAKE, out)
  | _ -> Error (`Fatal `InvalidSession)
