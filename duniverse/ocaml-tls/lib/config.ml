open Utils
open Core

open Sexplib.Std

let src = Logs.Src.create "tls.config" ~doc:"TLS config"
module Log = (val Logs.src_log src : Logs.LOG)

type certchain = Cert.t list * Priv.t [@@deriving sexp_of]

type own_cert = [
  | `None
  | `Single of certchain
  | `Multiple of certchain list
  | `Multiple_default of certchain * certchain list
] [@@deriving sexp_of]

type session_cache = SessionID.t -> epoch_data option
let sexp_of_session_cache _ = Sexplib.Sexp.Atom "SESSION_CACHE"

module Auth = struct
  type t = X509.Authenticator.t
  let sexp_of_t _ = Sexplib.Sexp.Atom "Authenticator"
end

module DN = struct
  type t = X509.Distinguished_name.t
  let sexp_of_t _ = Sexplib.Sexp.Atom "distinguished name"
end

type ticket_cache = {
  lookup : Cstruct.t -> (psk13 * epoch_data) option ;
  ticket_granted : psk13 -> epoch_data -> unit ;
  lifetime : int32 ;
  timestamp : unit -> Ptime.t
}

type ticket_cache_opt = ticket_cache option
let sexp_of_ticket_cache_opt _ = Sexplib.Sexp.Atom "TICKET_CACHE"

(* TODO: min_rsa, min_dh *)
type config = {
  ciphers : Ciphersuite.ciphersuite list ;
  protocol_versions : tls_version * tls_version ;
  signature_algorithms : signature_algorithm list ;
  use_reneg : bool ;
  authenticator : Auth.t option ;
  peer_name : Peer_name.t option ;
  own_certificates : own_cert ;
  acceptable_cas : DN.t list ;
  session_cache : session_cache ;
  ticket_cache : ticket_cache_opt ;
  cached_session : epoch_data option ;
  cached_ticket : (psk13 * epoch_data) option ;
  alpn_protocols : string list ;
  groups : group list ;
  zero_rtt : int32 ;
} [@@deriving sexp_of]

let ciphers13 cfg =
  List.rev
    (List.fold_left (fun acc cs ->
         match Ciphersuite.ciphersuite_to_ciphersuite13 cs with
         | None -> acc
         | Some c -> c :: acc)
        [] cfg.ciphers)

module Ciphers = struct

  (* A good place for various pre-baked cipher lists and helper functions to
   * slice and groom those lists. *)

  let default13 = [
    `AES_128_GCM_SHA256 ;
    `AES_256_GCM_SHA384 ;
    `CHACHA20_POLY1305_SHA256 ;
    `AES_128_CCM_SHA256 ;
  ]

  let default = default13 @ [
    `DHE_RSA_WITH_AES_256_GCM_SHA384 ;
    `DHE_RSA_WITH_AES_128_GCM_SHA256 ;
    `DHE_RSA_WITH_AES_256_CCM ;
    `DHE_RSA_WITH_AES_128_CCM ;
    `DHE_RSA_WITH_CHACHA20_POLY1305_SHA256 ;
    `ECDHE_RSA_WITH_AES_128_GCM_SHA256 ;
    `ECDHE_RSA_WITH_AES_256_GCM_SHA384 ;
    `ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256 ;
    `ECDHE_ECDSA_WITH_AES_128_GCM_SHA256 ;
    `ECDHE_ECDSA_WITH_AES_256_GCM_SHA384 ;
    `ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256 ;
    ]

  let supported = default @ [
      `DHE_RSA_WITH_AES_256_CBC_SHA256 ;
      `DHE_RSA_WITH_AES_128_CBC_SHA256 ;
      `DHE_RSA_WITH_AES_256_CBC_SHA ;
      `DHE_RSA_WITH_AES_128_CBC_SHA ;
      `ECDHE_RSA_WITH_AES_256_CBC_SHA384 ;
      `ECDHE_RSA_WITH_AES_128_CBC_SHA256 ;
      `ECDHE_RSA_WITH_AES_256_CBC_SHA ;
      `ECDHE_RSA_WITH_AES_128_CBC_SHA ;
      `ECDHE_ECDSA_WITH_AES_128_CBC_SHA ;
      `ECDHE_ECDSA_WITH_AES_256_CBC_SHA ;
      `ECDHE_ECDSA_WITH_AES_128_CBC_SHA256 ;
      `ECDHE_ECDSA_WITH_AES_256_CBC_SHA384 ;
      `RSA_WITH_AES_256_CBC_SHA256 ;
      `RSA_WITH_AES_128_CBC_SHA256 ;
      `RSA_WITH_AES_256_CBC_SHA ;
      `RSA_WITH_AES_128_CBC_SHA ;
      `RSA_WITH_AES_256_GCM_SHA384 ;
      `RSA_WITH_AES_128_GCM_SHA256 ;
      `RSA_WITH_AES_256_CCM ;
      `RSA_WITH_AES_128_CCM ;
      `DHE_RSA_WITH_3DES_EDE_CBC_SHA ;
      `RSA_WITH_3DES_EDE_CBC_SHA ;
      `ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA ;
    ]

  (* as defined in https://httpwg.org/specs/rfc7540.html#BadCipherSuites *)
  let http2 = default13 @ [
    `DHE_RSA_WITH_AES_256_GCM_SHA384 ;
    `DHE_RSA_WITH_AES_128_GCM_SHA256 ;
    `DHE_RSA_WITH_AES_256_CCM ;
    `DHE_RSA_WITH_AES_128_CCM ;
    `DHE_RSA_WITH_CHACHA20_POLY1305_SHA256 ;
    `ECDHE_RSA_WITH_AES_128_GCM_SHA256 ;
    `ECDHE_RSA_WITH_AES_256_GCM_SHA384 ;
    `ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256 ;
    `ECDHE_ECDSA_WITH_AES_128_GCM_SHA256 ;
    `ECDHE_ECDSA_WITH_AES_256_GCM_SHA384 ;
    `ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256 ;
  ]

  let fs_of = List.filter Ciphersuite.ciphersuite_fs

  let fs = fs_of default
end

let default_signature_algorithms =
  [ `ECDSA_SECP256R1_SHA256 ;
    `ECDSA_SECP384R1_SHA384 ;
    `ECDSA_SECP521R1_SHA512 ;
    `ED25519 ;
    `RSA_PSS_RSAENC_SHA256 ;
    `RSA_PSS_RSAENC_SHA384 ;
    `RSA_PSS_RSAENC_SHA512 ;
    `RSA_PKCS1_SHA256 ;
    `RSA_PKCS1_SHA384 ;
    `RSA_PKCS1_SHA512 ;
  ]

let supported_signature_algorithms =
  default_signature_algorithms @ [
    `RSA_PKCS1_SHA224 ;
    `ECDSA_SECP256R1_SHA1 ;
    `RSA_PKCS1_SHA1 ;
    `RSA_PKCS1_MD5
  ]

let min_dh_size = 1024

let min_rsa_key_size = 1024

let supported_groups =
  [ `X25519 ; `P384 ; `P256 ; `P521 ;
    `FFDHE2048 ; `FFDHE3072 ; `FFDHE4096 ; `FFDHE6144 ; `FFDHE8192 ]

let elliptic_curve = function
  | `X25519 | `P256 | `P384 | `P521 -> true
  | `FFDHE2048 | `FFDHE3072 | `FFDHE4096 | `FFDHE6144 | `FFDHE8192 -> false

let default_config = {
  ciphers = Ciphers.default ;
  protocol_versions = (`TLS_1_2, `TLS_1_3) ;
  signature_algorithms = default_signature_algorithms ;
  use_reneg = false ;
  authenticator = None ;
  peer_name = None ;
  own_certificates = `None ;
  acceptable_cas = [] ;
  session_cache = (fun _ -> None) ;
  cached_session = None ;
  cached_ticket = None ;
  alpn_protocols = [] ;
  groups = supported_groups ;
  ticket_cache = None ;
  zero_rtt = 0l ;
}

(* There are inter-configuration option constraints that are checked and
   adjusted here. The overall approach is if the client explicitly provided
   values, these are taken as granted (a conflict will result in an error). If
   the defaults are used, they are adjusted depending on the others.

   The options in question are:
   - ciphers, which before 1.3 include the key exchange (FFDHE, ECDHE, RSA)
   - groups, which name the FFDHE and ECDHE groups used for DH
   - signature_algorithms, which (since 1.2) specify the key type and algorithm
     used for signatures (RSA-PKCS, RSA-PSS, ECDSA/EdDSA)
   - certificate chains, which influence ciphers (before 1.3) and
     signature_algorithms

   Using everywhere the default (but a custom certificate / or multiple) result
   in a working configuration (where, depending on the certificate key type,
   some signature_algorithms and ciphersuites are removed). The provided server
   certificate may remove ciphers & signature_algorithms, but will only result
   in failure if these will then be empty.

   An invalid configuration is for example: only FFDHE ciphersuites, but no
   FFDHE groups. Or only EC signature algorithms, but only ciphers where the
   key type is RSA.

   At session initiation time, the server implementation selects cipher,
   certificate, signature_algorithm, and group depending on its configuration
   and client request.
*)


let invalid msg = invalid_arg ("Tls.Config: invalid configuration: " ^ msg)

let ciphers_and_groups ?ciphers ?groups default_ciphers =
  let tls13 = function #Ciphersuite.ciphersuite13 -> true | _ -> false in
  match ciphers, groups with
  | None, None -> default_ciphers, supported_groups
  | Some cs, None ->
    cs,
    let has_kex x = function
      | #Ciphersuite.ciphersuite13 -> true
      | c -> x = Ciphersuite.ciphersuite_kex c
    in
    begin
      match List.exists (has_kex `ECDHE) cs, List.exists (has_kex `FFDHE) cs with
      | true, true -> supported_groups
      | true, false ->
        Log.warn (fun m -> m "removed FFDHE groups (no FFDHE ciphersuite) from configuation");
        List.filter elliptic_curve supported_groups
      | false, true ->
        Log.warn (fun m -> m "removed ECDHE groups (no ECDHE ciphersuite) from configuration");
        List.filter (fun g -> not (elliptic_curve g)) supported_groups
      | false, false -> []
    end
  | None, Some g ->
    begin match List.partition elliptic_curve g with
      | [], [] ->
        Log.warn (fun m -> m "removed DHE and ECDHE ciphersuites (empty groups provided) from configuration");
        List.filter (fun c -> not (Ciphersuite.ciphersuite_fs c)) default_ciphers
      | _::_, [] ->
        Log.warn (fun m -> m "removed DHE ciphersuites (no FFDHE groups provided) from configuration");
        List.filter (fun c -> not (Ciphersuite.dhe_only c)) default_ciphers
      | [], _ :: _ ->
        Log.warn (fun m -> m "removed ECDHE ciphersuites (no EC groups provided) from configuration");
        List.filter (fun c -> not (Ciphersuite.ecdhe_only c)) default_ciphers
      | _ -> default_ciphers
    end, g
  | Some cs, Some g ->
    if List.exists Ciphersuite.ecdhe_only cs && not (List.exists elliptic_curve g) then
      invalid "ciphersuite with ECDHE provided, but no EC group";
    if List.exists Ciphersuite.dhe_only cs && not (List.exists (fun g -> not (elliptic_curve g)) g) then
      invalid "ciphersuite with FFDHE provided, but no FF group";
    if List.exists Ciphersuite.ciphersuite_fs cs && g = [] then
      invalid "ciphersuite with forward security provided, but no group";
    if List.exists elliptic_curve g && not (List.exists Ciphersuite.ecdhe cs) then
      invalid "EC group provided, but no ciphersuite with ECDHE";
    if List.exists (fun g -> not (elliptic_curve g)) g &&
       not (List.exists (fun c -> Ciphersuite.dhe_only c || tls13 c) cs)
    then
      invalid "FF group provided, but no ciphersuite with DHE";
    cs, g

let ciphers_and_sig_alg ?ciphers ?signature_algorithms default_ciphers =
  let tls13 = function #Ciphersuite.ciphersuite13 -> true | _ -> false in
  let default_sa_from_ciphers c =
    let has_key k c = tls13 c || k = Ciphersuite.ciphersuite_keytype c in
    match List.exists (has_key `RSA) c, List.exists (has_key `EC) c with
    | true, true -> supported_signature_algorithms
    | true, false ->
      Log.warn (fun m -> m "removed EC signature algorithms (no EC ciphersuite present)");
      List.filter rsa_sigalg supported_signature_algorithms
    | false, true ->
      Log.warn (fun m -> m "removed RSA signature algorithms (no RSA ciphersuite present)");
      List.filter (fun sa -> not (rsa_sigalg sa)) supported_signature_algorithms
    | false, false ->
      invalid "ciphersuite list without RSA and EC keys"
  in
  match ciphers, signature_algorithms with
  | None, None -> default_ciphers, default_sa_from_ciphers default_ciphers
  | Some c, None -> c, default_sa_from_ciphers c
  | None, Some sa ->
    begin match List.partition rsa_sigalg sa with
      | [], [] -> invalid "empty signature algorithms provided"
      | _::_, [] ->
        Log.warn (fun m -> m "removing EC ciphers (no EC signature algorithm provided)");
        List.filter
          (fun c -> tls13 c || not (Ciphersuite.ciphersuite_keytype c = `EC))
          default_ciphers,
        sa
      | [], _::_ ->
        Log.warn (fun m -> m "removing RSA ciphers (no RSA signature algorithm provided)");
        List.filter
          (fun c -> tls13 c || not (Ciphersuite.ciphersuite_keytype c = `RSA))
          default_ciphers,
        sa
      | _::_, _::_ -> default_ciphers, sa
    end
  | Some c, Some sa ->
    if List.exists rsa_sigalg sa && not (List.exists (fun c -> Ciphersuite.ciphersuite_keytype c = `RSA) c) then
      invalid "RSA signature algorithm, but no ciphersuites with RSA keys";
    if List.exists (fun s -> not (rsa_sigalg s)) sa && not (List.exists (fun c -> Ciphersuite.ciphersuite_keytype c = `EC) c) then
      invalid "EC signature algorithm, but no ciphersuites with EC keys";
    if List.exists (fun c -> Ciphersuite.ciphersuite_keytype c = `RSA) c && not (List.exists rsa_sigalg sa) then
      invalid "RSA ciphersuite, but no RSA signature algorithm";
    if List.exists (fun c -> Ciphersuite.ciphersuite_keytype c = `EC) c && not (List.exists (fun s -> not (rsa_sigalg s)) sa) then
      invalid "EC ciphersuite, but no EC signature algorithm";
    c, sa

let validate_common config =
  let (v_min, v_max) = config.protocol_versions in
  if v_max < v_min then invalid "bad version range" ;
  let ciphers, signature_algorithms =
    match v_min, v_max with
    | _, `TLS_1_1 | _, `TLS_1_0 ->
      Log.warn (fun m -> m "TLS 1.0 or TLS 1.1 as maximum version configured, removing 1.2 and 1.3 ciphersuites");
      List.filter (fun c ->
          not (Ciphersuite.ciphersuite_tls12_only c || Ciphersuite.ciphersuite_tls13 c))
        config.ciphers,
      []
    | _, `TLS_1_2 ->
      if config.signature_algorithms = [] then
        invalid "TLS 1.2 configured but no signature algorithms provided"
      else begin
        Log.warn (fun m -> m "TLS 1.2 as maximum version configured, removing 1.3 cipher suites");
        List.filter
          (fun c -> not (Ciphersuite.ciphersuite_tls13 c)) config.ciphers,
        config.signature_algorithms
      end
    | `TLS_1_3, `TLS_1_3 ->
      let sa = List.filter tls13_sigalg config.signature_algorithms in
      if sa = [] then
        invalid "TLS 1.3 configured but no 1.3 signature algorithms provided"
      else begin
        Log.warn (fun m -> m "only TLS 1.3 configured, removing pre-1.3 cipher suites and signature algorithms");
        List.filter Ciphersuite.ciphersuite_tls13 config.ciphers, sa
      end
    | _ -> config.ciphers, config.signature_algorithms
  in
  if not (List_set.is_proper_set ciphers) then
    invalid "set of ciphers is not a proper set" ;
  if List.length ciphers = 0 then
    invalid "set of ciphers is empty" ;
  if not (List_set.is_proper_set config.groups) then
    invalid "set of groups is not a proper set" ;
  if not (List_set.is_proper_set signature_algorithms) then
    invalid "set of signature algorithms is not a proper set" ;
  if List.exists (fun proto -> let len = String.length proto in len = 0 || len > 255) config.alpn_protocols then
    invalid "invalid alpn protocol" ;
  if List.length config.alpn_protocols > 0xffff then
    invalid "alpn protocols list too large" ;
  { config with ciphers ; signature_algorithms }

let validate_certificate_chain = function
  | (s::chain, priv) ->
    let pub = X509.Private_key.public priv in
    ( match pub with
      | `RSA pub ->
        if Mirage_crypto_pk.Rsa.pub_bits pub < min_rsa_key_size then
          invalid "RSA key too short!"
      | _ -> () );
    ( let eq_pub a b =
        Cstruct.equal
          (X509.Public_key.fingerprint a)
          (X509.Public_key.fingerprint b)
      in
      if not (eq_pub pub (X509.Certificate.public_key s)) then
          invalid "public / private key combination" ) ;
     ( match init_and_last chain with
       | Some (ch, trust) ->
         (* TODO: verify that certificates are x509 v3 if TLS_1_2 *)
         ( match X509.Validation.verify_chain_of_trust ~time:(fun () -> None) ~host:None ~anchors:[trust] (s :: ch) with
           | Ok _   -> ()
           | Error x ->
             let s = Fmt.to_to_string X509.Validation.pp_validation_error x in
             invalid ("certificate chain does not validate: " ^ s))
       | None -> () )
  | _ -> invalid "certificate chain"

let validate_client config =
  match config.own_certificates with
  | `None -> ()
  | `Single c -> validate_certificate_chain c
  | _ -> invalid_arg "multiple client certificates not supported in client config"

let non_overlapping cs =
  let namessets =
    filter_map cs ~f:(function
        | (s :: _, _) -> Some s
        | _           -> None)
    |> List.map X509.Certificate.hostnames
  in
  let rec check = function
    | []    -> ()
    | s::ss ->
      if not (List.for_all (fun ss' ->
          X509.Host.Set.is_empty (X509.Host.Set.inter s ss'))
          ss)
      then
        invalid_arg "overlapping names in certificates"
      else
        check ss
  in
  check namessets

module KU = Set.Make (struct
    type t = X509.Extension.key_usage
    let compare a b = compare a b
  end)

module PK = Map.Make (struct
    type t = [ `RSA | `ED25519 | `P256 | `P384 | `P521 ]
    let compare a b = compare a b
  end)

let validate_server config =
  let open Ciphersuite in
  let usages =
    List.fold_left
      (fun acc c -> KU.add (required_usage (ciphersuite_kex c)) acc)
      KU.empty config.ciphers
  and certificate_chains =
    match config.own_certificates with
    | `Single c                 -> [c]
    | `Multiple cs              -> cs
    | `Multiple_default (c, cs) -> c :: cs
    | `None                     -> invalid "no server certificate provided"
  in
  let server_certs =
    List.map (function (s::_,_) -> s | _ -> invalid "empty certificate chain")
      certificate_chains
  in
  if not
      (KU.for_all (fun u ->
           List.exists (supports_key_usage ~not_present:true u) server_certs)
          usages)
  then
    invalid "certificate usage does not match" ;
  List.iter validate_certificate_chain certificate_chains ;
  let rsa_cert, ec_cert =
    let is_ec_cert c = match X509.Certificate.public_key c with
      | `ED25519 _ | `P256 _ | `P384 _ | `P521 _ -> true
      | _ -> false
    and is_rsa_cert c = match X509.Certificate.public_key c with
      | `RSA _ -> true | _ -> false
    in
    List.exists is_rsa_cert server_certs,
    List.exists is_ec_cert server_certs
  in
  let ciphers =
    List.filter
      (function
        | #Ciphersuite.ciphersuite13 -> true
        | c ->
          let keytype = ciphersuite_keytype c in
          (rsa_cert && keytype = `RSA) || (ec_cert && keytype = `EC))
      config.ciphers
  in
  ( match config.own_certificates with
    | `Multiple cs
    | `Multiple_default (_, cs) ->
      let add k v acc = match PK.find_opt k acc with
        | None -> PK.add k [v] acc
        | Some r -> PK.add k (v :: r) acc
      in
      let pk =
        List.fold_left (fun acc cs ->
            match snd cs with
            | `RSA _ -> add `RSA cs acc
            | `ED25519 _ -> add `ED25519 cs acc
            | `P256 _ -> add `P256 cs acc
            | `P384 _ -> add `P384 cs acc
            | `P521 _ -> add `P521 cs acc
            | _ -> invalid "unknown key type")
          PK.empty cs
      in
      PK.iter (fun _ chains -> non_overlapping chains) pk
    | _ -> () );
  { config with ciphers }

let validate_keys_sig_algs config =
  let _, v_max = config.protocol_versions in
  if v_max = `TLS_1_2 || v_max = `TLS_1_3 then
    let certificate_chains =
      match config.own_certificates with
      | `Single c                 -> [c]
      | `Multiple cs              -> cs
      | `Multiple_default (c, cs) -> c :: cs
      | `None                     -> invalid "no server certificate provided"
    in
    let server_keys =
      List.map (function
          | (s::_,_) -> X509.Certificate.public_key s
          | _ -> invalid "empty certificate chain")
        certificate_chains
    in
    if not
        (List.for_all (fun cert ->
             List.exists (pk_matches_sa cert) config.signature_algorithms)
            server_keys)
    then
      invalid "certificate provided which does not allow any signature algorithm"

type client = config [@@deriving sexp_of]
type server = config [@@deriving sexp_of]

let client_of_sexp _ = invalid_arg "couldn't decode client configuration"
let server_of_sexp _ = invalid_arg "couldn't decode server configuration"

let of_server conf = conf
and of_client conf = conf

let peer conf name = { conf with peer_name = Some name }

let with_authenticator conf auth = { conf with authenticator = Some auth }

let with_own_certificates conf own_certificates = { conf with own_certificates }

let with_acceptable_cas conf acceptable_cas = { conf with acceptable_cas }

let (<?>) ma b = match ma with None -> b | Some a -> a

let client
  ~authenticator ?peer_name ?ciphers ?version ?signature_algorithms ?reneg ?certificates ?cached_session ?cached_ticket ?ticket_cache ?alpn_protocols ?groups () =
  let ciphers', groups = ciphers_and_groups ?ciphers ?groups default_config.ciphers in
  let ciphers, signature_algorithms = ciphers_and_sig_alg ?ciphers ?signature_algorithms ciphers' in
  let config =
    { default_config with
        authenticator = Some authenticator ;
        ciphers ;
        protocol_versions = version <?> default_config.protocol_versions ;
        signature_algorithms ;
        use_reneg = reneg <?> default_config.use_reneg ;
        own_certificates  = certificates <?> default_config.own_certificates ;
        peer_name = peer_name ;
        cached_session = cached_session ;
        alpn_protocols = alpn_protocols <?> default_config.alpn_protocols ;
        ticket_cache = ticket_cache ;
        cached_ticket = cached_ticket ;
        groups ;
    } in
  let config = validate_common config in
  validate_client config ;
  Log.debug (fun m -> m "client with %s"
                (Sexplib.Sexp.to_string_hum (sexp_of_config config)));
  config

let server
    ?ciphers ?version ?signature_algorithms ?reneg ?certificates ?acceptable_cas ?authenticator ?session_cache ?ticket_cache ?alpn_protocols ?groups ?zero_rtt () =
  let ciphers', groups = ciphers_and_groups ?ciphers ?groups default_config.ciphers in
  let ciphers, signature_algorithms = ciphers_and_sig_alg ?ciphers ?signature_algorithms ciphers' in
  let config =
    { default_config with
        ciphers ;
        protocol_versions = version <?> default_config.protocol_versions ;
        signature_algorithms ;
        use_reneg = reneg <?> default_config.use_reneg ;
        own_certificates = certificates <?> default_config.own_certificates ;
        acceptable_cas = acceptable_cas <?> default_config.acceptable_cas ;
        authenticator = authenticator ;
        session_cache = session_cache  <?> default_config.session_cache ;
        alpn_protocols = alpn_protocols <?> default_config.alpn_protocols ;
        ticket_cache = ticket_cache ;
        groups ;
        zero_rtt = zero_rtt <?> default_config.zero_rtt ;
    } in
  let config = validate_server config in
  let config = validate_common config in
  validate_keys_sig_algs config;
  Log.debug (fun m -> m "server with %s"
                (Sexplib.Sexp.to_string_hum (sexp_of_config config)));
  config
