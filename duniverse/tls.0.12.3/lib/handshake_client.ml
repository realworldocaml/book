open Utils

open Core
open State
open Handshake_common
open Config

let (<+>) = Cs.(<+>)

let state_version state = match state.protocol_version with
  | #tls_before_13 as v -> v
  | _ -> assert false

let default_client_hello config =
  let host = match config.peer_name with
    | None   -> []
    | Some x -> [`Hostname x]
  in
  let version = max_protocol_version config.protocol_versions in
  let ecc_groups = match List.filter Config.elliptic_curve config.groups with
    | [] -> []
    | xs -> [ `SupportedGroups (List.map group_to_named_group xs) ]
  in
  let extensions, secrets = match version with
    | `TLS_1_0 | `TLS_1_1 -> (ecc_groups, [])
    | `TLS_1_2 ->
      (`SignatureAlgorithms config.signature_algorithms :: ecc_groups, [])
    | `TLS_1_3 ->
      let sig_alg = config.signature_algorithms (* TODO: filter deprecated ones *)
      and groups = List.map group_to_named_group config.groups
      and secrets, keyshares =
        (* OTOH, we could send all the keyshares (but this is pretty substantial size) *)
        (* instead we pick the first two groups and send keyshares *)
        let rec gen c gs acc = match c with
          | 0 -> List.rev acc
          | _ -> match gs with
            | [] -> List.rev acc (* TODO log? complain? *)
            | g::gs' ->
              let priv, share = Handshake_crypto13.dh_gen_key g in
              let acc' = ((g, priv),(group_to_named_group g, share)) :: acc in
              gen (pred c) gs' acc'
        in
        List.split (gen 2 config.groups [])
      in
      let all = all_versions config.protocol_versions in
      let supported_versions = List.map (fun x -> (x :> tls_any_version)) all in
      let exts =
        [`SignatureAlgorithms sig_alg ; `SupportedGroups groups ; `KeyShare keyshares ; `SupportedVersions supported_versions ]
      in
      (exts, secrets)
  in
  let alpn = match config.alpn_protocols with
    | [] -> []
    | protocols -> [`ALPN protocols]
  in
  let ciphers =
    match
      min_protocol_version config.protocol_versions,
      max_protocol_version config.protocol_versions
    with
    | `TLS_1_3, _ -> (ciphers13 config :> Ciphersuite.ciphersuite list)
    | _, `TLS_1_3 -> config.ciphers
    | _, `TLS_1_1 | _, `TLS_1_0 -> List.filter (o not Ciphersuite.ciphersuite_tls12_only) config.ciphers
    | _ -> config.ciphers
  and sessionid =
    match config.use_reneg, config.cached_session with
    | _, Some { session_id ; extended_ms ; _ } when extended_ms && not (Cs.null session_id) -> Some session_id
    | false, Some { session_id ; _ } when not (Cs.null session_id) -> Some session_id
    | _ -> None
  in
  let ch = {
    client_version = (version :> tls_any_version) ;
    client_random  = Mirage_crypto_rng.generate 32 ;
    sessionid      = sessionid ;
    ciphersuites   = List.map Ciphersuite.ciphersuite_to_any_ciphersuite ciphers ;
    extensions     = `ExtendedMasterSecret :: host @ extensions @ alpn
  }
  in
  (ch, version, secrets)

let common_server_hello_validation config reneg (sh : server_hello) (ch : client_hello) =
  let validate_reneg data =
    match reneg, data with
    | Some (cvd, svd), Some x -> guard (Cs.equal (cvd <+> svd) x) (`Fatal `InvalidRenegotiation)
    | Some _, None -> fail (`Fatal `NoSecureRenegotiation)
    | None, Some x -> guard (Cs.null x) (`Fatal `InvalidRenegotiation)
    | None, None -> return ()
  in
  guard (List.mem sh.ciphersuite config.ciphers)
    (`Error (`NoConfiguredCiphersuite [sh.ciphersuite])) >>= fun () ->
  guard (server_hello_valid sh &&
         server_exts_subset_of_client sh.extensions ch.extensions)
    (`Fatal `InvalidServerHello) >>= fun () ->
  (match get_alpn_protocol sh with
   | None -> return ()
   | Some x ->
     guard (List.mem x config.alpn_protocols) (`Fatal `InvalidServerHello)) >>= fun () ->
  validate_reneg (get_secure_renegotiation sh.extensions)

let common_server_hello_machina state (sh : server_hello) (ch : client_hello) raw log =
  let cipher = sh.ciphersuite in
  let session_id = match sh.sessionid with None -> Cstruct.create 0 | Some x -> x in
  let extended_ms =
    List.mem `ExtendedMasterSecret ch.extensions &&
    List.mem `ExtendedMasterSecret sh.extensions
  in
  let alpn_protocol = get_alpn_protocol sh in
  let session =
    let session = empty_session in
    let common_session_data = {
      session.common_session_data with
      client_random    = ch.client_random ;
      server_random    = sh.server_random ;
      alpn_protocol ;
    } in {
      session with
      common_session_data ;
      ciphersuite      = cipher ;
      session_id ;
      extended_ms ;
      client_version   = ch.client_version ;
    }
  in
  let state = { state with protocol_version = sh.server_version } in
  match Ciphersuite.ciphersuite_kex cipher with
  | `RSA     ->
    let machina = Client (AwaitCertificate_RSA (session, log @ [raw])) in
    Ok ({ state with machina }, [])
  | `DHE_RSA ->
    let machina = Client (AwaitCertificate_DHE_RSA (session, log @ [raw])) in
    Ok ({ state with machina }, [])
  | _ ->
    (* we don't support PSK for 1.2 and below *)
    Error (`Fatal `UnsupportedKeyExchange)

let answer_server_hello state (ch : client_hello) sh secrets raw log =
  let validate_version requested (lo, _) server_version =
    guard (version_ge requested server_version && server_version >= lo)
      (`Error (`NoConfiguredVersions [ server_version ]))
  in

  let cfg = state.config in
  common_server_hello_validation cfg None sh ch >>= fun () ->
  validate_version ch.client_version state.config.protocol_versions sh.server_version >>= fun () ->

  (if max_protocol_version state.config.protocol_versions = `TLS_1_3 then
     let piece = Cstruct.sub sh.server_random 24 8 in
     guard (not (Cstruct.equal Packet.downgrade12 piece)) (`Fatal `Downgrade12) >>= fun () ->
     guard (not (Cstruct.equal Packet.downgrade11 piece)) (`Fatal `Downgrade11)
   else
     return ()) >>= fun () ->

  let epoch_matches (epoch : epoch_data) =
    epoch.ciphersuite = sh.ciphersuite &&
      epoch.protocol_version = sh.server_version &&
        option false (SessionID.equal epoch.session_id) sh.sessionid &&
          (not cfg.use_reneg ||
             (List.mem `ExtendedMasterSecret sh.extensions && epoch.extended_ms))
  in

  Tracing.sexpf ~tag:"version" ~f:sexp_of_tls_version sh.server_version ;
  trace_cipher sh.ciphersuite ;

  let state = { state with protocol_version = sh.server_version } in
  match sh.server_version with
  | #tls13 ->
    Handshake_client13.answer_server_hello state ch sh secrets raw (Cs.appends log)
  | #tls_before_13 as v ->
    match state.config.cached_session with
    | Some epoch when epoch_matches epoch ->
      let session =
        let session = session_of_epoch epoch in
        let common_session_data = {
          session.common_session_data with
          client_random = ch.client_random ;
          server_random = sh.server_random ;
          client_auth = match epoch.own_certificate with [] -> false | _ -> true ;
        } in
        { session with
          common_session_data ;
          client_version = ch.client_version ;
        }
      in
      let client_ctx, server_ctx =
        Handshake_crypto.initialise_crypto_ctx v session
      in
      let machina = AwaitServerChangeCipherSpecResume (session, client_ctx, server_ctx, log @ [raw]) in
      Ok ({ state with machina = Client machina }, [])
    | _ -> common_server_hello_machina state sh ch raw log

let answer_server_hello_renegotiate state session (ch : client_hello) sh raw log =
  common_server_hello_validation state.config (Some session.renegotiation) sh ch >>= fun () ->
  guard (state.protocol_version = sh.server_version)
    (`Fatal (`InvalidRenegotiationVersion sh.server_version)) >>= fun () ->
  common_server_hello_machina state sh ch raw log

let validate_keytype_usage certificate ciphersuite =
  let keytype, usage =
    Ciphersuite.(o required_keytype_and_usage ciphersuite_kex ciphersuite)
  in
  match certificate with
  | None -> fail (`Fatal `NoCertificateReceived)
  | Some cert ->
    let open X509 in
    guard (Certificate.supports_keytype cert keytype)
      (`Fatal `NotRSACertificate) >>= fun () ->
    guard (supports_key_usage ~not_present:true cert usage)
      (`Fatal `InvalidCertificateUsage) >>= fun () ->
    guard
      (supports_extended_key_usage cert `Server_auth ||
       supports_extended_key_usage ~not_present:true cert `Any)
      (`Fatal `InvalidCertificateExtendedUsage)

let answer_certificate_RSA state (session : session_data) cs raw log =
  let cfg = state.config in
  validate_chain cfg.authenticator cs (host_name_opt cfg.peer_name) >>= fun (peer_certificate, received_certificates, peer_certificate_chain, trust_anchor) ->
  validate_keytype_usage peer_certificate session.ciphersuite >>= fun () ->
  let session =
    let common_session_data = { session.common_session_data with received_certificates ; peer_certificate ; peer_certificate_chain ; trust_anchor } in
    { session with common_session_data }
  in
  ( match session.client_version with
    | `TLS_1_3 -> return `TLS_1_2
    | #tls_before_13 as v -> return v
    | x -> fail (`Fatal (`NoVersions [ x ])) (* TODO: get rid of this... *)
  ) >>= fun version ->
  let ver = Writer.assemble_protocol_version version in
  let premaster = ver <+> Mirage_crypto_rng.generate 46 in
  peer_rsa_key peer_certificate >|= fun pubkey ->
  let kex = Mirage_crypto_pk.Rsa.PKCS1.encrypt ~key:pubkey premaster in
  let kex = Writer.assemble_client_dh_key_exchange kex in
  let machina =
    AwaitCertificateRequestOrServerHelloDone
      (session, kex, premaster, log @ [raw])
  in
  ({ state with machina = Client machina }, [])

let answer_certificate_DHE_RSA state (session : session_data) cs raw log =
  let cfg = state.config in
  validate_chain cfg.authenticator cs (host_name_opt cfg.peer_name) >>= fun (peer_certificate, received_certificates, peer_certificate_chain, trust_anchor) ->
  validate_keytype_usage peer_certificate session.ciphersuite >|= fun () ->
  let session =
    let common_session_data = { session.common_session_data with received_certificates ; peer_certificate ; peer_certificate_chain ; trust_anchor } in
    { session with common_session_data }
  in
  let machina = AwaitServerKeyExchange_DHE_RSA (session, log @ [raw]) in
  ({ state with machina = Client machina }, [])

let answer_server_key_exchange_DHE_RSA state (session : session_data) kex raw log =
  let to_fatal r = match r with Ok cs -> return cs | Error er -> fail (`Fatal (`ReaderError er)) in
  (if Ciphersuite.ecc session.ciphersuite then
     to_fatal (Reader.parse_ec_parameters kex) >|= fun (g, share, raw, left) ->
     let g = match g with | `X25519 -> `Hacl `X25519 | `P256 -> `Fiat `P256 in
     (g, share, raw, left)
   else
     let unpack_dh dh_params =
       match Crypto.dh_params_unpack dh_params with
       | Ok data -> return data
       | Error (`Msg m) -> fail (`Fatal (`ReaderError (Unknown m)))
     in
     to_fatal (Reader.parse_dh_parameters kex) >>= fun (dh_params, raw_dh_params, leftover) ->
     unpack_dh dh_params >>= fun (group, shared) ->
     guard (Mirage_crypto_pk.Dh.modulus_size group >= Config.min_dh_size)
       (`Fatal `InsufficientDH) >|= fun () ->
     (`Mirage_crypto group, shared, raw_dh_params, leftover)
  ) >>= fun (group, shared, raw_dh_params, leftover) ->

  let sigdata = session.common_session_data.client_random <+> session.common_session_data.server_random <+> raw_dh_params in
  verify_digitally_signed state.protocol_version state.config.signature_algorithms leftover sigdata session.common_session_data.peer_certificate >>= fun () ->

  (match group with
   | `Mirage_crypto g ->
     let secret, client_share = Mirage_crypto_pk.Dh.gen_key g in
     begin match Mirage_crypto_pk.Dh.shared secret shared with
       | None     -> fail (`Fatal `InvalidDH)
       | Some pms -> return (pms, Writer.assemble_client_dh_key_exchange client_share)
     end
   | `Fiat `P256 ->
     let secret, client_share = Fiat_p256.gen_key ~rng:Mirage_crypto_rng.generate in
     begin match Fiat_p256.key_exchange secret shared with
       | Error _ -> fail (`Fatal `InvalidDH)
       | Ok pms -> return (pms, Writer.assemble_client_ec_key_exchange client_share)
     end
   | `Hacl `X25519 ->
     let secret, client_share = Hacl_x25519.gen_key ~rng:Mirage_crypto_rng.generate in
     begin match Hacl_x25519.key_exchange secret shared with
       | Error _ -> fail (`Fatal `InvalidDH)
       | Ok pms -> return (pms, Writer.assemble_client_ec_key_exchange client_share)
     end
  ) >|= fun (pms, kex) ->
  let machina =
    AwaitCertificateRequestOrServerHelloDone
      (session, kex, pms, log @ [raw])
  in
  { state with machina = Client machina }, []

let answer_certificate_request state (session : session_data) cr kex pms raw log =
  let cfg = state.config in
  ( match state.protocol_version with
    | `TLS_1_0 | `TLS_1_1 ->
       ( match Reader.parse_certificate_request cr with
         | Ok (types, cas) -> return (types, None, cas)
         | Error re -> fail (`Fatal (`ReaderError re)) )
    | `TLS_1_2 ->
       ( match Reader.parse_certificate_request_1_2 cr with
         | Ok (types, sigalgs, cas) -> return (types, Some sigalgs, cas)
         | Error re -> fail (`Fatal (`ReaderError re)) )
    | v -> fail (`Fatal (`BadRecordVersion (v :> tls_any_version))) (* never happens *)
  ) >|= fun (types, sigalgs, _cas) ->
  (* TODO: respect cas, maybe multiple client certificates? *)
  let own_certificate, own_private_key =
    match
      List.mem Packet.RSA_SIGN types,
      cfg.own_certificates
    with
    | true, `Single (chain, priv) -> (chain, Some priv)
    | _ -> ([], None)
  in
  let session =
    let common_session_data = {
      session.common_session_data with
      own_certificate ;
      own_private_key ;
      client_auth = true
    } in
    { session with common_session_data }
  in
  let machina = AwaitServerHelloDone (session, sigalgs, kex, pms, log @ [raw]) in
  ({ state with machina = Client machina }, [])

let answer_server_hello_done state (session : session_data) sigalgs kex premaster raw log =
  let kex = ClientKeyExchange kex in
  let ckex = Writer.assemble_handshake kex in

  ( match session.common_session_data.client_auth, session.common_session_data.own_private_key with
    | true, Some p ->
       let cs = List.map X509.Certificate.encode_der session.common_session_data.own_certificate in
       let cert = Certificate (Writer.assemble_certificates cs) in
       let ccert = Writer.assemble_handshake cert in
       let to_sign = log @ [ raw ; ccert ; ckex ] in
       let data = Cs.appends to_sign in
       let ver = state.protocol_version
       and my_sigalgs = state.config.signature_algorithms in
       signature ver data sigalgs my_sigalgs p >|= fun (signature) ->
       let cert_verify = CertificateVerify signature in
       let ccert_verify = Writer.assemble_handshake cert_verify in
       ([ cert ; kex ; cert_verify ],
        [ ccert ; ckex ; ccert_verify ],
        to_sign, Some ccert_verify)
    | true, None ->
       let cert = Certificate (Writer.assemble_certificates []) in
       let ccert = Writer.assemble_handshake cert in
       return ([cert ; kex], [ccert ; ckex], log @ [ raw ; ccert ; ckex ], None)
    | false, _ ->
       return ([kex], [ckex], log @ [ raw ; ckex ], None) )
  >|= fun (msgs, raw_msgs, raws, cert_verify) ->

  let to_fin = raws @ option [] (fun x -> [x]) cert_verify in

  let master_secret =
    Handshake_crypto.derive_master_secret (state_version state) session premaster raws
  in
  let session =
    let common_session_data = { session.common_session_data with master_secret } in
    { session with common_session_data }
  in
  let client_ctx, server_ctx =
    Handshake_crypto.initialise_crypto_ctx (state_version state) session
  in

  let checksum = Handshake_crypto.finished (state_version state) session.ciphersuite master_secret "client finished" to_fin in
  let fin = Finished checksum in
  let raw_fin = Writer.assemble_handshake fin in
  let ps = to_fin @ [raw_fin] in

  let session =
    let common_session_data = { session.common_session_data with master_secret } in
    { session with common_session_data }
  in
  let machina = AwaitServerChangeCipherSpec (session, server_ctx, checksum, ps)
  and ccst, ccs = change_cipher_spec in

  List.iter (Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake) msgs;
  Tracing.cs ~tag:"change-cipher-spec-out" ccs ;
  Tracing.cs ~tag:"master-secret" master_secret;
  Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake fin;

  ({ state with machina = Client machina },
   List.map (fun x -> `Record (Packet.HANDSHAKE, x)) raw_msgs @
     [ `Record (ccst, ccs);
       `Change_enc client_ctx;
       `Record (Packet.HANDSHAKE, raw_fin)])

let answer_server_finished state (session : session_data) client_verify fin log =
  let computed =
    Handshake_crypto.finished (state_version state) session.ciphersuite session.common_session_data.master_secret "server finished" log
  in
  guard (Cs.equal computed fin) (`Fatal `BadFinished) >>= fun () ->
  guard (Cs.null state.hs_fragment) (`Fatal `HandshakeFragmentsNotEmpty) >|= fun () ->
  let machina = Established
  and session = { session with renegotiation = (client_verify, computed) } in
  ({ state with machina = Client machina ; session = `TLS session :: state.session }, [])

let answer_server_finished_resume state (session : session_data) fin raw log =
  let client, server =
    let checksum = Handshake_crypto.finished (state_version state) session.ciphersuite session.common_session_data.master_secret in
    (checksum "client finished" (log @ [raw]), checksum "server finished" log)
  in
  guard (Cs.equal server fin) (`Fatal `BadFinished) >>= fun () ->
  guard (Cs.null state.hs_fragment) (`Fatal `HandshakeFragmentsNotEmpty) >|= fun () ->
  let machina = Established
  and session = { session with renegotiation = (client, server) }
  in
  let finished = Finished client in
  let raw_finished = Writer.assemble_handshake finished in
  Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake finished ;
  ({ state with machina = Client machina ; session = `TLS session :: state.session },
   [`Record (Packet.HANDSHAKE, raw_finished)])

let answer_hello_request state =
  let produce_client_hello session config exts =
     let dch, _, _ = default_client_hello config in
     let ch = { dch with extensions = dch.extensions @ exts ; sessionid = None } in
     let raw = Writer.assemble_handshake (ClientHello ch) in
     let machina = AwaitServerHelloRenegotiate (session, ch, [raw]) in
     Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake (ClientHello ch) ;
     ({ state with machina = Client machina }, [`Record (Packet.HANDSHAKE, raw)])
  in

  match state.config.use_reneg, state.session with
  | true , `TLS x :: _ ->
    let ext = `SecureRenegotiation (fst x.renegotiation) in
    return (produce_client_hello x state.config [ext])
  | true , _      -> fail (`Fatal `InvalidSession) (* I'm pretty sure this can be an assert false *)
  | false, _      ->
    let no_reneg = Writer.assemble_alert ~level:Packet.WARNING Packet.NO_RENEGOTIATION in
    Tracing.sexpf ~tag:"alert-out" ~f:sexp_of_tls_alert (Packet.WARNING, Packet.NO_RENEGOTIATION) ;
    return (state, [`Record (Packet.ALERT, no_reneg)])

let handle_change_cipher_spec cs state packet =
  match Reader.parse_change_cipher_spec packet, cs with
  | Ok (), AwaitServerChangeCipherSpec (session, server_ctx, client_verify, log) ->
     guard (Cs.null state.hs_fragment) (`Fatal `HandshakeFragmentsNotEmpty) >|= fun () ->
     let machina = AwaitServerFinished (session, client_verify, log) in
     Tracing.cs ~tag:"change-cipher-spec-in" packet ;
     ({ state with machina = Client machina }, [`Change_dec server_ctx])
  | Ok (), AwaitServerChangeCipherSpecResume (session, client_ctx, server_ctx, log) ->
     guard (Cs.null state.hs_fragment) (`Fatal `HandshakeFragmentsNotEmpty) >|= fun () ->
     let ccs = change_cipher_spec in
     let machina = AwaitServerFinishedResume (session, log) in
     Tracing.cs ~tag:"change-cipher-spec-in" packet ;
     Tracing.cs ~tag:"change-cipher-spec-out" packet ;
     ({ state with machina = Client machina },
      [`Record ccs ; `Change_enc client_ctx; `Change_dec server_ctx])
  | Error re, _ -> fail (`Fatal (`ReaderError re))
  | _ -> fail (`Fatal `UnexpectedCCS)

let handle_handshake cs hs buf =
  let open Reader in
  match parse_handshake buf with
  | Ok handshake ->
     Tracing.sexpf ~tag:"handshake-in" ~f:sexp_of_tls_handshake handshake ;
     ( match cs, handshake with
       | AwaitServerHello (ch, secrets, log), ServerHello sh ->
          answer_server_hello hs ch sh secrets buf log
       | AwaitServerHello (ch, secrets, log), HelloRetryRequest hrr ->
          Handshake_client13.answer_hello_retry_request hs ch hrr secrets buf (Cs.appends log)
       | AwaitServerHelloRenegotiate (session, ch, log), ServerHello sh ->
          answer_server_hello_renegotiate hs session ch sh buf log
       | AwaitCertificate_RSA (session, log), Certificate cs ->
          (match Reader.parse_certificates cs with
           | Ok cs -> answer_certificate_RSA hs session cs buf log
           | Error re -> fail (`Fatal (`ReaderError re)))
       | AwaitCertificate_DHE_RSA (session, log), Certificate cs ->
          (match Reader.parse_certificates cs with
           | Ok cs -> answer_certificate_DHE_RSA hs session cs buf log
           | Error re -> fail (`Fatal (`ReaderError re)))
       | AwaitServerKeyExchange_DHE_RSA (session, log), ServerKeyExchange kex ->
          answer_server_key_exchange_DHE_RSA hs session kex buf log
       | AwaitCertificateRequestOrServerHelloDone (session, kex, pms, log), CertificateRequest cr ->
          answer_certificate_request hs session cr kex pms buf log
       | AwaitCertificateRequestOrServerHelloDone (session, kex, pms, log), ServerHelloDone ->
          answer_server_hello_done hs session None kex pms buf log
       | AwaitServerHelloDone (session, sigalgs, kex, pms, log), ServerHelloDone ->
          answer_server_hello_done hs session sigalgs kex pms buf log
       | AwaitServerFinished (session, client_verify, log), Finished fin ->
          answer_server_finished hs session client_verify fin log
       | AwaitServerFinishedResume (session, log), Finished fin ->
          answer_server_finished_resume hs session fin buf log
       | Established, HelloRequest ->
          answer_hello_request hs
       | _, hs -> fail (`Fatal (`UnexpectedHandshake hs)) )
  | Error re -> fail (`Fatal (`ReaderError re))
