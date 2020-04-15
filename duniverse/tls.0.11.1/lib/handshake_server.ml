open Utils

open Core
open State
open Handshake_common
open Config

let (<+>) = Cs.(<+>)

let hello_request state =
  if state.config.use_reneg then
    let hr = HelloRequest in
    (* Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake hr ; *)
    let state = { state with machina = Server AwaitClientHelloRenegotiate } in
    return (state, [`Record (Packet.HANDSHAKE, Writer.assemble_handshake hr)])
  else
    fail (`Fatal `InvalidSession)


let answer_client_finished state session client_fin raw log =
  let client, server =
    let checksum = Handshake_crypto.finished state.protocol_version session.ciphersuite session.master_secret in
    (checksum "client finished" log, checksum "server finished" (log @ [raw]))
  in
  guard (Cs.equal client client_fin) (`Fatal `BadFinished) >>= fun () ->
  let fin = Finished server in
  let fin_raw = Writer.assemble_handshake fin in
  (* we really do not want to have any leftover handshake fragments *)
  guard (Cs.null state.hs_fragment) (`Fatal `HandshakeFragmentsNotEmpty) >|= fun () ->
  let session = { session with renegotiation = (client, server) }
  and machina = Server Established
  in
  (* Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake fin ; *)
  ({ state with machina ; session = session :: state.session },
   [`Record (Packet.HANDSHAKE, fin_raw)])

let answer_client_finished_resume state session server_verify client_fin _raw log =
  let client_verify =
    Handshake_crypto.finished state.protocol_version session.ciphersuite session.master_secret "client finished" log
  in
  guard (Cs.equal client_verify client_fin) (`Fatal `BadFinished) >>= fun () ->
  (* we really do not want to have any leftover handshake fragments *)
  guard (Cs.null state.hs_fragment) (`Fatal `HandshakeFragmentsNotEmpty) >|= fun () ->
  let session = { session with renegotiation = (client_verify, server_verify) }
  and machina = Server Established
  in
  (* Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake fin ; *)
  ({ state with machina ; session = session :: state.session }, [])

let establish_master_secret state session premastersecret raw log =
  let log = log @ [raw] in
  let master_secret = Handshake_crypto.derive_master_secret
      state.protocol_version session premastersecret log
  in
  let session = { session with master_secret = master_secret } in
  let client_ctx, server_ctx =
    Handshake_crypto.initialise_crypto_ctx state.protocol_version session
  in
  let machina =
    match session.peer_certificate with
    | None -> AwaitClientChangeCipherSpec (session, server_ctx, client_ctx, log)
    | Some _ -> AwaitClientCertificateVerify (session, server_ctx, client_ctx, log)
  in
  (* Tracing.cs ~tag:"master-secret" master_secret ; *)
  ({ state with machina = Server machina }, [])

let private_key session =
  match session.own_private_key with
    | Some priv -> return priv
    | None      -> fail (`Fatal `InvalidSession) (* TODO: assert false / ensure via typing in config *)

let validate_certs certs authenticator session =
  validate_chain authenticator certs None >|= fun (peer_certificate, received_certificates, peer_certificate_chain, trust_anchor) ->
  { session with received_certificates ; peer_certificate ; peer_certificate_chain ; trust_anchor }

let answer_client_certificate_RSA state session certs raw log =
  validate_certs certs state.config.authenticator session >|= fun session ->
  let machina = AwaitClientKeyExchange_RSA (session, log @ [raw]) in
  ({ state with machina = Server machina }, [])

let answer_client_certificate_DHE_RSA state session dh_sent certs raw log =
  validate_certs certs state.config.authenticator session >|= fun session ->
  let machina = AwaitClientKeyExchange_DHE_RSA (session, dh_sent, log @ [raw]) in
  ({ state with machina = Server machina }, [])

let answer_client_certificate_verify state session sctx cctx verify raw log =
  let sigdata = Cs.appends log in
  verify_digitally_signed state.protocol_version state.config.hashes verify sigdata session.peer_certificate >|= fun () ->
  let machina = AwaitClientChangeCipherSpec (session, sctx, cctx, log @ [raw]) in
  ({ state with machina = Server machina }, [])

let answer_client_key_exchange_RSA state session kex raw log =
  (* due to bleichenbacher attach, we should use a random pms *)
  (* then we do not leak any decryption or padding errors! *)
  let other = Writer.assemble_protocol_version state.protocol_version <+> Mirage_crypto_rng.generate 46 in
  let validate_premastersecret k =
    (* Client implementations MUST always send the correct version number in
       PreMasterSecret.  If ClientHello.client_version is TLS 1.1 or higher,
       server implementations MUST check the version number as described in
       the note below.  If the version number is TLS 1.0 or earlier, server
       implementations SHOULD check the version number, but MAY have a
       configuration option to disable the check.  Note that if the check
       fails, the PreMasterSecret SHOULD be randomized as described below *)
    (* we do not provide an option to disable the version checking (yet!) *)
    match Cstruct.len k == 48, Reader.parse_any_version k with
    | true, Ok c_ver when c_ver = session.client_version -> k
    | _                                                                  -> other
  in

  private_key session >|= fun priv ->

  let pms = match Mirage_crypto_pk.Rsa.PKCS1.decrypt ~key:priv kex with
    | None   -> validate_premastersecret other
    | Some k -> validate_premastersecret k
  in
  establish_master_secret state session pms raw log

let answer_client_key_exchange_DHE_RSA state session secret kex raw log =
  match Mirage_crypto_pk.Dh.shared secret kex with
  | None     -> fail (`Fatal `InvalidDH)
  | Some pms -> return (establish_master_secret state session pms raw log)

let sig_algs (client_hello : client_hello) =
  map_find client_hello.extensions ~f:(function
      | `SignatureAlgorithms xs -> Some xs
      | _                       -> None)

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

let agreed_cipher cert requested =
  let type_usage_matches cipher =
    let cstyp, csusage =
      Ciphersuite.(required_keytype_and_usage @@ ciphersuite_kex cipher)
    in
    X509.(Certificate.supports_keytype cert cstyp &&
          supports_key_usage ~not_present:true cert csusage)
  in
  List.filter type_usage_matches requested

let server_hello session version reneg =
  (* RFC 4366: server shall reply with an empty hostname extension *)
  let host = option [] (fun _ -> [`Hostname]) session.own_name
  and server_random = Mirage_crypto_rng.generate 32
  and secren = match reneg with
    | None            -> `SecureRenegotiation (Cstruct.create 0)
    | Some (cvd, svd) -> `SecureRenegotiation (cvd <+> svd)
  and ems = if session.extended_ms then
      [`ExtendedMasterSecret]
    else
      []
  and session_id =
    match Cstruct.len session.session_id with
    | 0 -> Mirage_crypto_rng.generate 32
    | _ -> session.session_id
  and alpn =
    match session.alpn_protocol with
    | None -> []
    | Some protocol -> [`ALPN protocol]
  in
  let sh = ServerHello
      { server_version = version ;
        server_random  = server_random ;
        sessionid      = Some session_id ;
        ciphersuite    = session.ciphersuite ;
        extensions     = secren :: host @ ems @ alpn }
  in
  (* Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake sh ; *)
  (Writer.assemble_handshake sh,
   { session with server_random ; session_id })

let answer_client_hello_common state reneg ch raw =
  let process_client_hello ch config =
    let host = hostname ch
    and cciphers = filter_map ~f:Ciphersuite.any_ciphersuite_to_ciphersuite ch.ciphersuites
    and tst = Ciphersuite.(o needs_certificate ciphersuite_kex) in
    ( if List.for_all tst cciphers then
        agreed_cert config.own_certificates host >>= function
        | (c::cs, priv) -> let cciphers = agreed_cipher c cciphers in
                           return (cciphers, c::cs, Some priv)
        | _ -> fail (`Fatal `InvalidSession) (* TODO: assert false / remove by types in config *)
      else
        return (cciphers, [], None) ) >>= fun (cciphers, chain, priv) ->

    ( match first_match cciphers config.ciphers with
      | Some x -> return x
      | None   -> match first_match cciphers Config.Ciphers.supported with
        | Some _ -> fail (`Error (`NoConfiguredCiphersuite cciphers))
        | None -> fail (`Fatal (`NoCiphersuite ch.ciphersuites)) ) >>= fun cipher ->

    let extended_ms = List.mem `ExtendedMasterSecret ch.extensions in

    (* Tracing.sexpf ~tag:"cipher" ~f:Ciphersuite.sexp_of_ciphersuite cipher ; *)

    ( match config.alpn_protocols, get_alpn_protocols ch with
      | _, None | [], _ -> return None
      | configured, Some client -> match first_match client configured with
        | Some proto -> return (Some proto)
        | None ->
          (* RFC7301 Section 3.2:
             In the event that the server supports no protocols that the client
             advertises, then the server SHALL respond with a fatal
             "no_application_protocol" alert. *)
          fail (`Fatal `NoApplicationProtocol) ) >|= fun alpn_protocol ->

    let own_name = match host with None -> None | Some h -> Some (Domain_name.to_string h) in
    { empty_session with
      client_random    = ch.client_random ;
      client_version   = ch.client_version ;
      ciphersuite      = cipher ;
      own_certificate  = chain ;
      own_private_key  = priv ;
      own_name         = own_name ;
      extended_ms      = extended_ms ;
      alpn_protocol    = alpn_protocol }

  and server_cert session =
    match session.own_certificate with
    | []    -> []
    | certs ->
       let cert = Certificate (List.map X509.Certificate.encode_der certs) in
       (* Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake cert ; *)
       [ Writer.assemble_handshake cert ]

  and cert_request version config session =
    let open Writer in
    match config.authenticator with
    | None -> ([], session)
    | Some _ ->
       let cas =
         List.map X509.Distinguished_name.encode_der config.acceptable_cas
       in
       let certreq = match version with
         | TLS_1_0 | TLS_1_1 ->
            let data = assemble_certificate_request [Packet.RSA_SIGN] cas in
            CertificateRequest data
         | TLS_1_2 ->
            let sigalgs = List.map (fun h -> (h, Packet.RSA)) config.hashes in
            let data =
              assemble_certificate_request_1_2 [Packet.RSA_SIGN] sigalgs cas
            in
            CertificateRequest data
       in
       (* Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake certreq ; *)
       ([ assemble_handshake certreq ], { session with client_auth = true })

  and kex_dhe_rsa config session version sig_algs =
    let group         = Config.dh_group in
    let (secret, msg) = Mirage_crypto_pk.Dh.gen_key group in
    let written =
      let dh_param = Crypto.dh_params_pack group msg in
      Writer.assemble_dh_parameters dh_param in

    let data = session.client_random <+> session.server_random <+> written in

    private_key session >>= fun priv ->
    signature version data sig_algs config.hashes priv >|= fun sgn ->
    let kex = ServerKeyExchange (written <+> sgn) in
    let hs = Writer.assemble_handshake kex in
    (* Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake kex ; *)
    (hs, secret) in

  process_client_hello ch state.config >>= fun session ->
  let sh, session = server_hello session state.protocol_version reneg in
  let certificates = server_cert session
  and cert_req, session = cert_request state.protocol_version state.config session
  and hello_done = Writer.assemble_handshake ServerHelloDone
  in

  ( match Ciphersuite.ciphersuite_kex session.ciphersuite with
    | Ciphersuite.DHE_RSA ->
        kex_dhe_rsa state.config session state.protocol_version (sig_algs ch) >>= fun (kex, dh) ->
        let outs = sh :: certificates @ [ kex ] @ cert_req @ [ hello_done ] in
        let log = raw :: outs in
        let machina =
          if session.client_auth then
            AwaitClientCertificate_DHE_RSA (session, dh, log)
          else
            AwaitClientKeyExchange_DHE_RSA (session, dh, log)
        in
        (* Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake ServerHelloDone ; *)
        return (outs, machina)
    | Ciphersuite.RSA ->
        let outs = sh :: certificates @ cert_req @ [ hello_done ] in
        let log = raw :: outs in
        let machina =
          if session.client_auth then
            AwaitClientCertificate_RSA (session, log)
          else
            AwaitClientKeyExchange_RSA (session, log)
        in
        (* Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake ServerHelloDone ; *)
        return (outs, machina)
    ) >|= fun (out_recs, machina) ->

  ({ state with machina = Server machina },
   [`Record (Packet.HANDSHAKE, Cs.appends out_recs)])

let agreed_version supported requested =
  match supported_protocol_version supported requested with
  | Some x -> return x
  | None   -> match requested with
    | Supported v -> fail (`Error (`NoConfiguredVersion v))
    | v -> fail (`Fatal (`NoVersion v))

let answer_client_hello state (ch : client_hello) raw =
  let ensure_reneg ciphers their_data  =
    let reneg_cs = List.mem Packet.TLS_EMPTY_RENEGOTIATION_INFO_SCSV ciphers in
    match reneg_cs, their_data with
    | _, Some x -> guard (Cs.null x) (`Fatal `InvalidRenegotiation)
    | true, _ -> return ()
    | _ -> fail (`Fatal `NoSecureRenegotiation)

  and resume (ch : client_hello) state =
    let epoch_matches (epoch : Core.epoch_data) version ciphers extensions =
      let cciphers = filter_map ~f:Ciphersuite.any_ciphersuite_to_ciphersuite ciphers in
      List.mem epoch.ciphersuite cciphers &&
        version = epoch.protocol_version &&
          (not state.config.use_reneg ||
             (List.mem `ExtendedMasterSecret extensions && epoch.extended_ms))
    in

    match option None state.config.session_cache ch.sessionid with
    | Some epoch when epoch_matches epoch state.protocol_version ch.ciphersuites ch.extensions ->
      let session = session_of_epoch epoch in
      Some { session with
             client_random = ch.client_random ;
             client_version = ch.client_version ;
             client_auth = (epoch.peer_certificate <> None) ;
           }
    | _ -> None

  and answer_resumption session state =
    let version = state.protocol_version in
    let sh, session = server_hello session version None in
    (* we really do not want to have any leftover handshake fragments *)
    guard (Cs.null state.hs_fragment) (`Fatal `HandshakeFragmentsNotEmpty) >|= fun () ->
    let client_ctx, server_ctx =
      Handshake_crypto.initialise_crypto_ctx version session
    in
    let ccs = change_cipher_spec in
    let log = [ raw ; sh ] in
    let server =
      Handshake_crypto.finished
        version session.ciphersuite session.master_secret "server finished" log
    in
    let fin = Finished server in
    let fin_raw = Writer.assemble_handshake fin in
    let machina = AwaitClientChangeCipherSpecResume (session, client_ctx, server, log @ [fin_raw]) in
    ({ state with machina = Server machina },
     [ `Record (Packet.HANDSHAKE, sh) ;
       `Record ccs ;
       `Change_enc (Some server_ctx) ;
       `Record (Packet.HANDSHAKE, fin_raw)])
  in

  let process_client_hello config ch =
    let cciphers = ch.ciphersuites in
    guard (client_hello_valid ch) (`Fatal `InvalidClientHello) >>= fun () ->
    agreed_version config.protocol_versions ch.client_version >>= fun version ->
    guard (not (List.mem Packet.TLS_FALLBACK_SCSV cciphers) ||
           version = max_protocol_version config.protocol_versions)
      (`Fatal `InappropriateFallback) >>= fun () ->
    let theirs = get_secure_renegotiation ch.extensions in
    ensure_reneg cciphers theirs >|= fun () ->

    (* Tracing.sexpf ~tag:"version" ~f:sexp_of_tls_version version ; *)

    version
  in

  process_client_hello state.config ch >>= fun protocol_version ->
  let state = { state with protocol_version } in
  match resume ch state with
  | None -> answer_client_hello_common state None ch raw
  | Some session -> answer_resumption session state

let answer_client_hello_reneg state (ch : client_hello) raw =
  (* ensure reneg allowed and supplied *)
  let ensure_reneg our_data their_data  =
    match our_data, their_data with
    | (cvd, _), Some x -> guard (Cs.equal cvd x) (`Fatal `InvalidRenegotiation)
    | _ -> fail (`Fatal `NoSecureRenegotiation)
  in

  let process_client_hello config oldversion ours ch =
    guard (client_hello_valid ch) (`Fatal `InvalidClientHello) >>= fun () ->
    agreed_version config.protocol_versions ch.client_version >>= fun version ->
    guard (version = oldversion) (`Fatal (`InvalidRenegotiationVersion version)) >>= fun () ->
    let theirs = get_secure_renegotiation ch.extensions in
    ensure_reneg ours theirs >|= fun () ->
    (* Tracing.sexpf ~tag:"version" ~f:sexp_of_tls_version version ; *)
    version
  in

  let config = state.config in
  match config.use_reneg, state.session with
  | true , session :: _  ->
     let reneg = session.renegotiation in
     process_client_hello config state.protocol_version reneg ch >>= fun _version ->
     answer_client_hello_common state (Some reneg) ch raw
  | false, _             ->
    let no_reneg = Writer.assemble_alert ~level:Packet.WARNING Packet.NO_RENEGOTIATION in
    return (state, [`Record (Packet.ALERT, no_reneg)])
  | true , _             -> fail (`Fatal `InvalidSession) (* I'm pretty sure this can be an assert false *)

let handle_change_cipher_spec ss state packet =
  match Reader.parse_change_cipher_spec packet, ss with
  | Ok (), AwaitClientChangeCipherSpec (session, server_ctx, client_ctx, log) ->
     guard (Cs.null state.hs_fragment) (`Fatal `HandshakeFragmentsNotEmpty)
     >>= fun () ->
     let ccs = change_cipher_spec in
     let machina = AwaitClientFinished (session, log)
     in
     Tracing.cs ~tag:"change-cipher-spec-in" packet ;
     Tracing.cs ~tag:"change-cipher-spec-out" packet ;

     return ({ state with machina = Server machina },
             [`Record ccs; `Change_enc (Some server_ctx); `Change_dec (Some client_ctx)])
  | Ok (), AwaitClientChangeCipherSpecResume (session, client_ctx, server_verify, log) ->
     guard (Cs.null state.hs_fragment) (`Fatal `HandshakeFragmentsNotEmpty) >|= fun () ->
     let machina = AwaitClientFinishedResume (session, server_verify, log)
     in
     Tracing.cs ~tag:"change-cipher-spec-in" packet ;
     Tracing.cs ~tag:"change-cipher-spec-out" packet ;

     ({ state with machina = Server machina },
      [`Change_dec (Some client_ctx)])
  | Error er, _ -> fail (`Fatal (`ReaderError er))
  | _ -> fail (`Fatal `UnexpectedCCS)

let handle_handshake ss hs buf =
  match Reader.parse_handshake buf with
  | Ok handshake ->
    (* Tracing.sexpf ~tag:"handshake-in" ~f:sexp_of_tls_handshake handshake; *)
     ( match ss, handshake with
       | AwaitClientHello, ClientHello ch ->
          answer_client_hello hs ch buf
       | AwaitClientCertificate_RSA (session, log), Certificate cs ->
          answer_client_certificate_RSA hs session cs buf log
       | AwaitClientCertificate_DHE_RSA (session, dh_sent, log), Certificate cs ->
          answer_client_certificate_DHE_RSA hs session dh_sent cs buf log
       | AwaitClientKeyExchange_RSA (session, log), ClientKeyExchange kex ->
          answer_client_key_exchange_RSA hs session kex buf log
       | AwaitClientKeyExchange_DHE_RSA (session, dh_sent, log), ClientKeyExchange kex ->
          answer_client_key_exchange_DHE_RSA hs session dh_sent kex buf log
       | AwaitClientCertificateVerify (session, sctx, cctx, log), CertificateVerify ver ->
          answer_client_certificate_verify hs session sctx cctx ver buf log
       | AwaitClientFinished (session, log), Finished fin ->
          answer_client_finished hs session fin buf log
       | AwaitClientFinishedResume (session, server_verify, log), Finished fin ->
          answer_client_finished_resume hs session server_verify fin buf log
       | Established, ClientHello ch -> (* client-initiated renegotiation *)
          answer_client_hello_reneg hs ch buf
       | AwaitClientHelloRenegotiate, ClientHello ch -> (* hello-request send, renegotiation *)
          answer_client_hello_reneg hs ch buf
       | _, hs -> fail (`Fatal (`UnexpectedHandshake hs)) )
  | Error re -> fail (`Fatal (`ReaderError re))
