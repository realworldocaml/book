open Utils

open Core
open State
open Handshake_common
open Config

open Rresult.R.Infix

let (<+>) = Cstruct.append

let state_version state = match state.protocol_version with
  | #tls_before_13 as v -> v
  | _ -> assert false

let hello_request state =
  if state.config.use_reneg then
    let hr = HelloRequest in
    Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake hr ;
    let state = { state with machina = Server AwaitClientHelloRenegotiate } in
    Ok (state, [`Record (Packet.HANDSHAKE, Writer.assemble_handshake hr)])
  else
    Error (`Fatal `InvalidSession)


let answer_client_finished state (session : session_data) client_fin raw log =
  let client, server =
    let checksum = Handshake_crypto.finished (state_version state)
        session.ciphersuite session.common_session_data.master_secret
    in
    (checksum "client finished" log, checksum "server finished" (log @ [raw]))
  in
  guard (Cstruct.equal client client_fin) (`Fatal `BadFinished) >>= fun () ->
  let fin = Finished server in
  let fin_raw = Writer.assemble_handshake fin in
  (* we really do not want to have any leftover handshake fragments *)
  guard (Cstruct.len state.hs_fragment = 0) (`Fatal `HandshakeFragmentsNotEmpty) >>| fun () ->
  let session = { session with renegotiation = (client, server) }
  and machina = Server Established
  in
  Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake fin ;
  ({ state with machina ; session = `TLS session :: state.session },
   [`Record (Packet.HANDSHAKE, fin_raw)])

let answer_client_finished_resume state (session : session_data) server_verify client_fin _raw log =
  let client_verify =
    Handshake_crypto.finished (state_version state) session.ciphersuite session.common_session_data.master_secret "client finished" log
  in
  guard (Cstruct.equal client_verify client_fin) (`Fatal `BadFinished) >>= fun () ->
  (* we really do not want to have any leftover handshake fragments *)
  guard (Cstruct.len state.hs_fragment = 0) (`Fatal `HandshakeFragmentsNotEmpty) >>| fun () ->
  let session = { session with renegotiation = (client_verify, server_verify) }
  and machina = Server Established
  in
  ({ state with machina ; session = `TLS session :: state.session }, [])

let establish_master_secret state (session : session_data) premastersecret raw log =
  let log = log @ [raw] in
  let master_secret = Handshake_crypto.derive_master_secret
      (state_version state) session premastersecret log
  in
  let session =
    let common_session_data = { session.common_session_data with master_secret } in
    { session with common_session_data }
  in
  let client_ctx, server_ctx =
    Handshake_crypto.initialise_crypto_ctx (state_version state) session
  in
  let machina =
    match session.common_session_data.peer_certificate with
    | None -> AwaitClientChangeCipherSpec (session, server_ctx, client_ctx, log)
    | Some _ -> AwaitClientCertificateVerify (session, server_ctx, client_ctx, log)
  in
  Tracing.cs ~tag:"master-secret" master_secret ;
  ({ state with machina = Server machina }, [])

let private_key (session : session_data) =
  match session.common_session_data.own_private_key with
    | Some priv -> Ok priv
    | None      -> Error (`Fatal `InvalidSession) (* TODO: assert false / ensure via typing in config *)

let validate_certs certs authenticator (session : session_data) =
  validate_chain authenticator certs None >>| fun (peer_certificate, received_certificates, peer_certificate_chain, trust_anchor) ->
  let common_session_data = {
    session.common_session_data with
    received_certificates ;
    peer_certificate ;
    peer_certificate_chain ;
    trust_anchor
  } in
  { session with common_session_data }

let answer_client_certificate_RSA state (session : session_data) certs raw log =
  validate_certs certs state.config.authenticator session >>| fun session ->
  let machina = AwaitClientKeyExchange_RSA (session, log @ [raw]) in
  ({ state with machina = Server machina }, [])

let answer_client_certificate_DHE state (session : session_data) dh_sent certs raw log =
  validate_certs certs state.config.authenticator session >>| fun session ->
  let machina = AwaitClientKeyExchange_DHE (session, dh_sent, log @ [raw]) in
  ({ state with machina = Server machina }, [])

let answer_client_certificate_verify state (session : session_data) sctx cctx verify raw log =
  let sigdata = Cstruct.concat log in
  verify_digitally_signed state.protocol_version state.config.signature_algorithms verify sigdata session.common_session_data.peer_certificate >>| fun () ->
  let machina = AwaitClientChangeCipherSpec (session, sctx, cctx, log @ [raw]) in
  ({ state with machina = Server machina }, [])

let answer_client_key_exchange_RSA state (session : session_data) kex raw log =
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
    | _ -> other
  in

  private_key session >>= function
  | `RSA key ->
    let pms = match Mirage_crypto_pk.Rsa.PKCS1.decrypt ~key kex with
      | None   -> validate_premastersecret other
      | Some k -> validate_premastersecret k
    in
    Ok (establish_master_secret state session pms raw log)
  | _ -> Error (`Fatal `NotRSACertificate)

let answer_client_key_exchange_DHE state session secret kex raw log =
  let to_fatal r = match r with Ok cs -> Ok cs | Error er -> Error (`Fatal (`ReaderError er)) in
  (let open Mirage_crypto_ec in
   match secret with
   | `P256 priv ->
     to_fatal (Reader.parse_client_ec_key_exchange kex) >>= fun share ->
     begin match P256.Dh.key_exchange priv share with
       | Error e -> Error (`Fatal (`BadECDH e))
       | Ok shared -> Ok shared
     end
   | `P384 priv ->
     to_fatal (Reader.parse_client_ec_key_exchange kex) >>= fun share ->
     begin match P384.Dh.key_exchange priv share with
       | Error e -> Error (`Fatal (`BadECDH e))
       | Ok shared -> Ok shared
     end
   | `P521 priv ->
     to_fatal (Reader.parse_client_ec_key_exchange kex) >>= fun share ->
     begin match P521.Dh.key_exchange priv share with
       | Error e -> Error (`Fatal (`BadECDH e))
       | Ok shared -> Ok shared
     end
   | `X25519 priv ->
     to_fatal (Reader.parse_client_ec_key_exchange kex) >>= fun share ->
     begin match X25519.key_exchange priv share with
       | Error e -> Error (`Fatal (`BadECDH e))
       | Ok shared -> Ok shared
     end
   | `Finite_field secret ->
     to_fatal (Reader.parse_client_dh_key_exchange kex) >>= fun share ->
     begin match Mirage_crypto_pk.Dh.shared secret share with
       | None -> Error (`Fatal `InvalidDH)
       | Some shared -> Ok shared
     end) >>| fun pms ->
  establish_master_secret state session pms raw log

let sig_algs (client_hello : client_hello) =
  map_find
    ~f:(function `SignatureAlgorithms xs -> Some xs | _ -> None)
    client_hello.extensions

let ecc_group configured_groups requested_groups =
  first_match requested_groups configured_groups

let agreed_cipher cert ecc requested =
  let usage_matches cipher =
    let csusage =
      Ciphersuite.(required_usage @@ ciphersuite_kex cipher)
    in
    supports_key_usage ~not_present:true csusage cert
  in
  let cciphers = List.filter usage_matches requested in
  if ecc then
    cciphers
  else
    List.filter (fun x -> not (Ciphersuite.ecdhe x)) cciphers

let server_hello config (client_hello : client_hello) (session : session_data) version reneg =
  (* RFC 4366: server shall reply with an empty hostname extension *)
  let host = option [] (fun _ -> [`Hostname]) session.common_session_data.own_name
  and server_random =
    let suffix =
      match version, max_protocol_version config.protocol_versions with
      | `TLS_1_2, `TLS_1_3 -> Packet.downgrade12
      | _, `TLS_1_3 -> Packet.downgrade11
      | _ -> Cstruct.create 0
    in
    let rst = Mirage_crypto_rng.generate (32 - Cstruct.len suffix) in
    rst <+> suffix
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
    match session.common_session_data.alpn_protocol with
    | None -> []
    | Some protocol -> [`ALPN protocol]
  and ecpointformat =
    match map_find ~f:(function `ECPointFormats -> Some () | _ -> None) client_hello.extensions with
    | Some () when Ciphersuite.ecdhe session.ciphersuite -> [ `ECPointFormats ]
    | _ -> []
  in
  let sh = ServerHello
      { server_version = version ;
        server_random  = server_random ;
        sessionid      = Some session_id ;
        ciphersuite    = session.ciphersuite ;
        extensions     = secren :: host @ ems @ alpn @ ecpointformat }
  in
  trace_cipher session.ciphersuite ;
  Tracing.sexpf ~tag:"version" ~f:sexp_of_tls_version version ;
  Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake sh ;
  let common_session_data = { session.common_session_data with server_random } in
  (Writer.assemble_handshake sh,
   { session with common_session_data ; session_id })

let answer_client_hello_common state reneg ch raw =
  let process_client_hello ch config =
    let host = hostname ch
    and groups = groups ch
    and cciphers = filter_map ~f:Ciphersuite.any_ciphersuite_to_ciphersuite ch.ciphersuites
    in
    let configured_ecc_groups, other_groups = List.partition Config.elliptic_curve config.groups in
    let ecc_group = ecc_group configured_ecc_groups groups
    and cciphers = List.filter (fun c -> not (Ciphersuite.ciphersuite_tls13 c)) cciphers
    in
    let cciphers = List.filter (fun c -> List.mem c config.ciphers) cciphers in
    let f =
      (* from the ciphers, figure out:
         - (a) RSA only (b) EC only
         - (c) static RSA only (keyUsage = KeyEncipherment) (d) DHE only (keyUsage = DigitalSignature)
         - (e) from the groups (they indicate the key type!)
      *)
      let kt_filter =
        match List.partition (fun c -> Ciphersuite.ciphersuite_keytype c = `RSA) cciphers with
        | _::_, [] -> begin fun s -> match X509.Certificate.public_key s with `RSA _ -> true | _ -> false end
        | [], _::_ -> begin fun s -> match X509.Certificate.public_key s with `ED25519 _ | `P256 _ | `P384 _ | `P521 _ -> true | _ -> false end
        | _, _ -> begin fun _s -> true end
      in
      let ku_filter =
        match List.partition (fun c -> Ciphersuite.ciphersuite_kex c = `RSA) cciphers with
        | _::_, [] -> supports_key_usage ~not_present:true `Key_encipherment
        | [], _::_ -> supports_key_usage ~not_present:true `Digital_signature
        | _ -> begin fun _ -> true end
      in
      let kt_matches_group s =
        match X509.Certificate.public_key s with
        | `RSA _ -> true
        | `ED25519 _ -> List.mem `X25519 groups
        | `P256 _ -> List.mem `P256 groups
        | `P384 _ -> List.mem `P384 groups
        | `P521 _ -> List.mem `P521 groups
        | _ -> false
      in
      fun s ->
        kt_filter s && ku_filter s && kt_matches_group s
    in
    let signature_algorithms = sig_algs ch in
    (agreed_cert ~f ?signature_algorithms config.own_certificates host >>= function
      | (c::cs, priv) -> let cciphers = agreed_cipher c (ecc_group <> None) cciphers in
                         Ok (cciphers, c::cs, Some priv)
      | ([], _) -> Error (`Fatal `InvalidSession) (* TODO: assert false / remove by types in config *)
    ) >>= fun (cciphers, chain, priv) ->

    ( match first_match cciphers config.ciphers with
      | Some x -> Ok x
      | None   -> match first_match cciphers Config.Ciphers.supported with
        | Some _ -> Error (`Error (`NoConfiguredCiphersuite cciphers))
        | None -> Error (`Fatal (`InvalidClientHello (`NoSupportedCiphersuite ch.ciphersuites))) ) >>= fun cipher ->

    let extended_ms = List.mem `ExtendedMasterSecret ch.extensions in

    Tracing.sexpf ~tag:"cipher" ~f:Ciphersuite.sexp_of_ciphersuite cipher ;

    alpn_protocol config ch >>| fun alpn_protocol ->

    let own_name = match host with None -> None | Some h -> Some (Domain_name.to_string h) in
    let group =
      if Ciphersuite.ecdhe cipher then
        ecc_group
      else match other_groups with
        | [] -> None
        | c::_ -> Some c
    in
    let session =
      let session = empty_session in
      let common_session_data = {
        session.common_session_data with
        client_random    = ch.client_random ;
        own_certificate  = chain ;
        own_private_key  = priv ;
        own_name         = own_name ;
        alpn_protocol    = alpn_protocol
      } in
      { session with
        common_session_data ;
        client_version   = ch.client_version ;
        ciphersuite      = cipher ;
        group            = group ;
        extended_ms      = extended_ms ;
      }
    in
    session

  and server_cert (session : session_data) =
    match session.common_session_data.own_certificate with
    | []    -> []
    | certs ->
       let cs = List.map X509.Certificate.encode_der certs in
       let cert = Certificate (Writer.assemble_certificates cs) in
       Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake cert ;
       [ Writer.assemble_handshake cert ]

  and cert_request version config (session : session_data) =
    let open Writer in
    match config.authenticator with
    | None -> Ok ([], session)
    | Some _ ->
       let cas =
         List.map X509.Distinguished_name.encode_der config.acceptable_cas
       and certs =
         [ Packet.RSA_SIGN ; Packet.ECDSA_SIGN ]
       in
       (match version with
        | `TLS_1_0 | `TLS_1_1 ->
          Ok (assemble_certificate_request certs cas)
        | `TLS_1_2 ->
          Ok (assemble_certificate_request_1_2 certs config.signature_algorithms cas)
        | `TLS_1_3 ->
          (* TLS 1.3 handshakes are diverted in answer_client_hello, this will
             never be executed. for renegotiation, it is checked that the
             protocol version did not change from the previous epoch (in
             answer_client_hello_reneg, process_client_hello the
             guard (version = oldversion)) *)
          Error (`Fatal (`BadRecordVersion (version :> tls_any_version)))) >>| fun data ->
       let certreq = CertificateRequest data in
       Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake certreq ;
       let common_session_data = { session.common_session_data with client_auth = true } in
       ([ assemble_handshake certreq ], { session with common_session_data })

  and kex_dhe config (session : session_data) version sig_algs =
    (match session.group with
     | None -> assert false (* can not happen *)
     | Some g ->
       let open Mirage_crypto_ec in
       match group_to_impl g with
       | `Finite_field g ->
         let (secret, msg) = Mirage_crypto_pk.Dh.gen_key g in
         let dh_param = Crypto.dh_params_pack g msg in
         let dh_params = Writer.assemble_dh_parameters dh_param in
         Ok (`Finite_field secret, dh_params)
       | `P256 ->
         let secret, shared = P256.Dh.gen_key () in
         let params = Writer.assemble_ec_parameters `P256 shared in
         Ok (`P256 secret, params)
       | `P384 ->
         let secret, shared = P384.Dh.gen_key () in
         let params = Writer.assemble_ec_parameters `P384 shared in
         Ok (`P384 secret, params)
       | `P521 ->
         let secret, shared = P521.Dh.gen_key () in
         let params = Writer.assemble_ec_parameters `P521 shared in
         Ok (`P521 secret, params)
       | `X25519 ->
         let secret, shared = X25519.gen_key () in
         let params = Writer.assemble_ec_parameters `X25519 shared in
         Ok (`X25519 secret, params)
    ) >>= fun (secret, written) ->
    let data = session.common_session_data.client_random <+> session.common_session_data.server_random <+> written in
    private_key session >>= fun priv ->
    signature version data sig_algs config.signature_algorithms priv >>| fun sgn ->
    let kex = ServerKeyExchange (written <+> sgn) in
    let hs = Writer.assemble_handshake kex in
    Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake kex ;
    (hs, secret) in

  process_client_hello ch state.config >>= fun session ->
  let sh, session = server_hello state.config ch session state.protocol_version reneg in
  let certificates = server_cert session
  and hello_done = Writer.assemble_handshake ServerHelloDone
  in
  cert_request state.protocol_version state.config session >>= fun (cert_req, session) ->

  ( match Ciphersuite.ciphersuite_kex session.ciphersuite with
    | #Ciphersuite.key_exchange_algorithm_dhe ->
        kex_dhe state.config session state.protocol_version (sig_algs ch) >>= fun (kex, dh) ->
        let outs = sh :: certificates @ [ kex ] @ cert_req @ [ hello_done ] in
        let log = raw :: outs in
        let machina =
          if session.common_session_data.client_auth then
            AwaitClientCertificate_DHE (session, dh, log)
          else
            AwaitClientKeyExchange_DHE (session, dh, log)
        in
        Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake ServerHelloDone ;
        Ok (outs, machina)
    | `RSA ->
        let outs = sh :: certificates @ cert_req @ [ hello_done ] in
        let log = raw :: outs in
        let machina =
          if session.common_session_data.client_auth then
            AwaitClientCertificate_RSA (session, log)
          else
            AwaitClientKeyExchange_RSA (session, log)
        in
        Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake ServerHelloDone ;
        Ok (outs, machina) ) >>| fun (out_recs, machina) ->

  ({ state with machina = Server machina },
   [`Record (Packet.HANDSHAKE, Cstruct.concat out_recs)])

(* TODO could benefit from result monadd *)
let agreed_version supported (client_hello : client_hello) =
  let raw_client_versions =
    match Utils.filter_map ~f:(function `SupportedVersions vs -> Some vs | _ -> None) client_hello.extensions with
    | [] -> [client_hello.client_version]
    | [vs] -> vs
    | _ -> invalid_arg "bad supported version extension"
  in
  let supported_versions = List.fold_left (fun acc v ->
      match any_version_to_version v with
      | None -> acc
      | Some v -> v :: acc) [] raw_client_versions
  in
  let client_versions = List.sort_uniq compare_tls_version supported_versions in
  match
    List.fold_left (fun r v ->
        match supported_protocol_version supported v with
        | None -> r
        | Some v -> Some v)
      None client_versions
  with
  | Some x -> Ok x
  | None   -> match supported_versions with
    | [] -> Error (`Fatal (`NoVersions raw_client_versions))
    | _ -> Error (`Error (`NoConfiguredVersions supported_versions))

let answer_client_hello state (ch : client_hello) raw =
  let ensure_reneg ciphers their_data  =
    let reneg_cs = List.mem Packet.TLS_EMPTY_RENEGOTIATION_INFO_SCSV ciphers in
    match reneg_cs, their_data with
    | _, Some x -> guard (Cstruct.len x = 0) (`Fatal `InvalidRenegotiation)
    | true, _ -> Ok ()
    | _ -> Error (`Fatal `NoSecureRenegotiation)

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
      let session =
        let session = session_of_epoch epoch in
        let common_session_data = {
          session.common_session_data with
          client_random = ch.client_random ;
          client_auth = (epoch.peer_certificate <> None) ;
        } in
        { session with common_session_data ; client_version = ch.client_version }
      in
      Some session
    | _ -> None

  and answer_resumption session state =
    let version = state_version state in
    let sh, session = server_hello state.config ch session version None in
    (* we really do not want to have any leftover handshake fragments *)
    guard (Cstruct.len state.hs_fragment = 0) (`Fatal `HandshakeFragmentsNotEmpty) >>| fun () ->
    let client_ctx, server_ctx =
      Handshake_crypto.initialise_crypto_ctx version session
    in
    let ccs = change_cipher_spec in
    let log = [ raw ; sh ] in
    let server =
      Handshake_crypto.finished
        version session.ciphersuite session.common_session_data.master_secret "server finished" log
    in
    let fin = Finished server in
    let fin_raw = Writer.assemble_handshake fin in
    Tracing.cs ~tag:"change-cipher-spec-out" (snd ccs) ;
    Tracing.sexpf ~tag:"handshake-out" ~f:sexp_of_tls_handshake fin ;
    let machina = AwaitClientChangeCipherSpecResume (session, client_ctx, server, log @ [fin_raw]) in
    ({ state with machina = Server machina },
     [ `Record (Packet.HANDSHAKE, sh) ;
       `Record ccs ;
       `Change_enc server_ctx ;
       `Record (Packet.HANDSHAKE, fin_raw)])
  in

  let process_client_hello config ch version =
    let cciphers = ch.ciphersuites in
    (match client_hello_valid version ch with
     | Ok () -> Ok ()
     | Error e -> Error (`Fatal (`InvalidClientHello e))) >>= fun () ->
    guard (not (List.mem Packet.TLS_FALLBACK_SCSV cciphers) ||
           version = max_protocol_version config.protocol_versions)
      (`Fatal `InappropriateFallback) >>= fun () ->
    let theirs = get_secure_renegotiation ch.extensions in
    ensure_reneg cciphers theirs
  in

  let process protocol_version =
    process_client_hello state.config ch protocol_version >>= fun () ->
    let state = { state with protocol_version } in
    (match resume ch state with
     | None -> answer_client_hello_common state None ch raw
     | Some session -> answer_resumption session state)
  in

  agreed_version state.config.protocol_versions ch >>= function
  | `TLS_1_3 -> Handshake_server13.answer_client_hello ~hrr:false state ch raw
  | protocol_version -> process protocol_version

let answer_client_hello_reneg state (ch : client_hello) raw =
  (* ensure reneg allowed and supplied *)
  let ensure_reneg our_data their_data  =
    match our_data, their_data with
    | (cvd, _), Some x -> guard (Cstruct.equal cvd x) (`Fatal `InvalidRenegotiation)
    | _ -> Error (`Fatal `NoSecureRenegotiation)
  in

  let process_client_hello config oldversion ours ch =
    (match client_hello_valid oldversion ch with
     | Ok () -> Ok ()
     | Error x -> Error (`Fatal (`InvalidClientHello x))) >>= fun () ->
    agreed_version config.protocol_versions ch >>= fun version ->
    guard (version = oldversion) (`Fatal (`InvalidRenegotiationVersion version)) >>= fun () ->
    let theirs = get_secure_renegotiation ch.extensions in
    ensure_reneg ours theirs >>| fun () ->
    version
  in

  let config = state.config in
  match config.use_reneg, state.session with
  | true , `TLS session :: _  ->
     let reneg = session.renegotiation in
     process_client_hello config state.protocol_version reneg ch >>= fun _version ->
     answer_client_hello_common state (Some reneg) ch raw
  | false, _             ->
    let no_reneg = Writer.assemble_alert ~level:Packet.WARNING Packet.NO_RENEGOTIATION in
    Tracing.sexpf ~tag:"alert-out" ~f:sexp_of_tls_alert (Packet.WARNING, Packet.NO_RENEGOTIATION) ;
    Ok (state, [`Record (Packet.ALERT, no_reneg)])
  | true , _             -> Error (`Fatal `InvalidSession) (* I'm pretty sure this can be an assert false *)

let handle_change_cipher_spec ss state packet =
  match Reader.parse_change_cipher_spec packet, ss with
  | Ok (), AwaitClientChangeCipherSpec (session, server_ctx, client_ctx, log) ->
     guard (Cstruct.len state.hs_fragment = 0) (`Fatal `HandshakeFragmentsNotEmpty)
     >>= fun () ->
     let ccs = change_cipher_spec in
     let machina = AwaitClientFinished (session, log)
     in
     Tracing.cs ~tag:"change-cipher-spec-in" packet ;
     Tracing.cs ~tag:"change-cipher-spec-out" packet ;

     Ok ({ state with machina = Server machina },
             [`Record ccs; `Change_enc server_ctx; `Change_dec client_ctx])
  | Ok (), AwaitClientChangeCipherSpecResume (session, client_ctx, server_verify, log) ->
     guard (Cstruct.len state.hs_fragment = 0) (`Fatal `HandshakeFragmentsNotEmpty) >>| fun () ->
     let machina = AwaitClientFinishedResume (session, server_verify, log)
     in
     Tracing.cs ~tag:"change-cipher-spec-in" packet ;

     ({ state with machina = Server machina },
      [`Change_dec client_ctx])
  | Error er, _ -> Error (`Fatal (`ReaderError er))
  | _ -> Error (`Fatal `UnexpectedCCS)

let handle_handshake ss hs buf =
  match Reader.parse_handshake buf with
  | Ok handshake ->
     Tracing.sexpf ~tag:"handshake-in" ~f:sexp_of_tls_handshake handshake;
     ( match ss, handshake with
       | AwaitClientHello, ClientHello ch ->
          answer_client_hello hs ch buf
       | AwaitClientCertificate_RSA (session, log), Certificate cs ->
          (match Reader.parse_certificates cs with
           | Ok cs -> answer_client_certificate_RSA hs session cs buf log
           | Error re -> Error (`Fatal (`ReaderError re)))
       | AwaitClientCertificate_DHE (session, dh_sent, log), Certificate cs ->
          (match Reader.parse_certificates cs with
           | Ok cs -> answer_client_certificate_DHE hs session dh_sent cs buf log
           | Error re -> Error (`Fatal (`ReaderError re)))
       | AwaitClientKeyExchange_RSA (session, log), ClientKeyExchange cs ->
          (match Reader.parse_client_dh_key_exchange cs with
           | Ok kex -> answer_client_key_exchange_RSA hs session kex buf log
           | Error re -> Error (`Fatal (`ReaderError re)))
       | AwaitClientKeyExchange_DHE (session, dh_sent, log), ClientKeyExchange kex ->
          answer_client_key_exchange_DHE hs session dh_sent kex buf log
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
       | _, hs -> Error (`Fatal (`UnexpectedHandshake hs)) )
  | Error re -> Error (`Fatal (`ReaderError re))
