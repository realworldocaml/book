open Utils
open Core

open Sexplib.Std

type certchain = Cert.t list * Mirage_crypto_pk.Rsa.priv [@@deriving sexp]

type own_cert = [
  | `None
  | `Single of certchain
  | `Multiple of certchain list
  | `Multiple_default of certchain * certchain list
] [@@deriving sexp]

type session_cache = SessionID.t -> epoch_data option
let session_cache_of_sexp _ = fun _ -> None
let sexp_of_session_cache _ = Sexplib.Sexp.Atom "SESSION_CACHE"

module Auth = struct
  type t = X509.Authenticator.t
  let t_of_sexp _ = failwith "can't convert sexp to authenticator"
  let sexp_of_t _ = Sexplib.Sexp.Atom "Authenticator"
end

module DN = struct
  type t = X509.Distinguished_name.t
  let t_of_sexp _ = failwith "can't convert sexp to distinguished name"
  let sexp_of_t _ = Sexplib.Sexp.Atom "distinguished name"
end

type ticket_cache = {
  lookup : Cstruct.t -> (psk13 * epoch_data) option ;
  ticket_granted : psk13 -> epoch_data -> unit ;
  lifetime : int32 ;
  timestamp : unit -> Ptime.t
}

type ticket_cache_opt = ticket_cache option
let ticket_cache_opt_of_sexp _ = None
let sexp_of_ticket_cache_opt _ = Sexplib.Sexp.Atom "TICKET_CACHE"

type config = {
  ciphers : Ciphersuite.ciphersuite list ;
  protocol_versions : tls_version * tls_version ;
  signature_algorithms : signature_algorithm list ;
  use_reneg : bool ;
  authenticator : Auth.t option ;
  peer_name : string option ;
  own_certificates : own_cert ;
  acceptable_cas : DN.t list ;
  session_cache : session_cache ;
  ticket_cache : ticket_cache_opt ;
  cached_session : epoch_data option ;
  cached_ticket : (psk13 * epoch_data) option ;
  alpn_protocols : string list ;
  groups : group list ;
  zero_rtt : int32 ;
} [@@deriving sexp]

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
    (* `CHACHA20_POLY1305_SHA256 ; *)
    `AES_128_CCM_SHA256 ;
  ]

  let default = default13 @ [
    `DHE_RSA_WITH_AES_256_GCM_SHA384 ;
    `DHE_RSA_WITH_AES_128_GCM_SHA256 ;
    `DHE_RSA_WITH_AES_256_CCM ;
    `DHE_RSA_WITH_AES_128_CCM ;
    `DHE_RSA_WITH_AES_256_CBC_SHA256 ;
    `DHE_RSA_WITH_AES_128_CBC_SHA256 ;
    `DHE_RSA_WITH_AES_256_CBC_SHA ;
    `DHE_RSA_WITH_AES_128_CBC_SHA ;
    `ECDHE_RSA_WITH_AES_128_GCM_SHA256 ;
    `ECDHE_RSA_WITH_AES_256_GCM_SHA384 ;
    `ECDHE_RSA_WITH_AES_256_CBC_SHA384 ;
    `ECDHE_RSA_WITH_AES_128_CBC_SHA256 ;
    `ECDHE_RSA_WITH_AES_256_CBC_SHA ;
    `ECDHE_RSA_WITH_AES_128_CBC_SHA ;
    `RSA_WITH_AES_256_GCM_SHA384 ;
    `RSA_WITH_AES_128_GCM_SHA256 ;
    `RSA_WITH_AES_256_CCM ;
    `RSA_WITH_AES_128_CCM ;
    `RSA_WITH_AES_256_CBC_SHA256 ;
    `RSA_WITH_AES_128_CBC_SHA256 ;
    `RSA_WITH_AES_256_CBC_SHA ;
    `RSA_WITH_AES_128_CBC_SHA ;
    ]

  let supported = default @ [
    `DHE_RSA_WITH_3DES_EDE_CBC_SHA ;
    `RSA_WITH_3DES_EDE_CBC_SHA ;
    ]

  let fs_of = List.filter Ciphersuite.ciphersuite_fs

  let fs = fs_of default
end

(* TODO split into <=12 and >=13, the SHA1 isn't 13 anymore *)
let default_signature_algorithms =
  [ `RSA_PSS_RSAENC_SHA256 ;
    `RSA_PSS_RSAENC_SHA384 ;
    `RSA_PSS_RSAENC_SHA512 ;
    `RSA_PKCS1_SHA256 ;
    `RSA_PKCS1_SHA384 ;
    `RSA_PKCS1_SHA512 ;
    `RSA_PKCS1_SHA1 ]

let supported_signature_algorithms =
  default_signature_algorithms @ [ `RSA_PKCS1_MD5 ]

let min_dh_size = 1024

let min_rsa_key_size = 1024

let supported_groups =
  [ `X25519 ; `P256 ; `FFDHE2048 ; `FFDHE3072 ; `FFDHE4096 ; `FFDHE6144 ; `FFDHE8192 ]

let elliptic_curve = function
  | `X25519 | `P256 -> true
  | _ -> false

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

let invalid msg = invalid_arg ("Tls.Config: invalid configuration: " ^ msg)

let validate_common config =
  let (v_min, v_max) = config.protocol_versions in
  if v_max < v_min then invalid "bad version range" ;
  ( match config.signature_algorithms with
    | [] when v_max >= `TLS_1_2 ->
       invalid "TLS 1.2 configured but no signature algorithms provided"
    | hs when not (List_set.subset hs supported_signature_algorithms) ->
       invalid "Some signature algorithms are not supported"
    | _ -> () ) ;
  if not (List_set.is_proper_set config.ciphers) then
    invalid "set of ciphers is not a proper set" ;
  if List.length config.ciphers = 0 then
    invalid "set of ciphers is empty" ;
  (* groups and ciphersuites (<= 1.2): any ECC if a ECDHE cipher present *)
  if List.exists Ciphersuite.ecc config.ciphers then
    if not (List.exists elliptic_curve config.groups) then
      invalid_arg "ECDHE ciphersuite configured, but no ECC curve";
  if List.exists (fun c -> Ciphersuite.(ciphersuite_fs c && not (ecc c))) config.ciphers then
    if List.for_all elliptic_curve config.groups then
      invalid_arg "DHE ciphersuites configured, but no FFDHE group";
  if List.exists (fun proto -> let len = String.length proto in len = 0 || len > 255) config.alpn_protocols then
    invalid "invalid alpn protocol" ;
  if List.length config.alpn_protocols > 0xffff then
    invalid "alpn protocols list too large"

module CertTypeUsageOrdered = struct
  type t = X509.Certificate.key_type * X509.Extension.key_usage
  let compare = compare
end
module CertTypeUsageSet = Set.Make(CertTypeUsageOrdered)

let validate_certificate_chain = function
  | (s::chain, priv) ->
     let pub = Mirage_crypto_pk.Rsa.pub_of_priv priv in
     if Mirage_crypto_pk.Rsa.pub_bits pub < min_rsa_key_size then
       invalid "RSA key too short!" ;
     ( match X509.Certificate.public_key s with
       | `RSA pub' when pub = pub' -> ()
       | _ -> invalid "public / private key combination" ) ;
     ( match init_and_last chain with
       | Some (ch, trust) ->
         (* TODO: verify that certificates are x509 v3 if TLS_1_2 *)
         ( match X509.Validation.verify_chain_of_trust ~time:(fun () -> None) ~host:None ~anchors:[trust] (s :: ch) with
           | Ok _   -> ()
           | Error x ->
             let s = Fmt.to_to_string X509.Validation.pp_validation_error x in
             invalid ("certificate chain does not validate: " ^ s))
       | None -> () )
  | _ -> invalid "certificate"

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

let validate_server config =
  let open Ciphersuite in
  let typeusage =
    let tylist =
      List.map ciphersuite_kex config.ciphers |>
        List.map required_keytype_and_usage
    in
    List.fold_right CertTypeUsageSet.add tylist CertTypeUsageSet.empty
  and certificate_chains =
    match config.own_certificates with
    | `Single c                 -> [c]
    | `Multiple cs              -> cs
    | `Multiple_default (c, cs) -> c :: cs
    | `None                     -> []
  in
  let server_certs =
    List.map (function
        | (s::_,_) -> s
        | _ -> invalid "empty certificate chain")
      certificate_chains
  in
  if
    not (CertTypeUsageSet.for_all
           (fun (t, u) ->
              List.exists (fun c ->
                  X509.Certificate.supports_keytype c t &&
                  supports_key_usage ~not_present:true c u)
                server_certs)
           typeusage)
  then
    invalid "certificate type or usage does not match" ;
  List.iter validate_certificate_chain certificate_chains ;
  ( match config.own_certificates with
    | `Multiple cs              -> non_overlapping cs
    | `Multiple_default (_, cs) -> non_overlapping cs
    | _                         -> () )
  (* TODO: verify that certificates are x509 v3 if TLS_1_2 *)


type client = config [@@deriving sexp]
type server = config [@@deriving sexp]

let of_server conf = conf
and of_client conf = conf

let peer conf name = { conf with peer_name = Some name }

let with_authenticator conf auth = { conf with authenticator = Some auth }

let with_own_certificates conf own_certificates = { conf with own_certificates }

let with_acceptable_cas conf acceptable_cas = { conf with acceptable_cas }

let (<?>) ma b = match ma with None -> b | Some a -> a

let client
  ~authenticator ?peer_name ?ciphers ?version ?signature_algorithms ?reneg ?certificates ?cached_session ?cached_ticket ?ticket_cache ?alpn_protocols ?groups () =
  let config =
    { default_config with
        authenticator = Some authenticator ;
        ciphers = ciphers <?> default_config.ciphers ;
        protocol_versions = version <?> default_config.protocol_versions ;
        signature_algorithms = signature_algorithms <?> default_config.signature_algorithms ;
        use_reneg = reneg <?> default_config.use_reneg ;
        own_certificates  = certificates <?> default_config.own_certificates ;
        peer_name = peer_name ;
        cached_session = cached_session ;
        alpn_protocols = alpn_protocols <?> default_config.alpn_protocols ;
        ticket_cache = ticket_cache ;
        cached_ticket = cached_ticket ;
        groups = groups <?> default_config.groups ;
    } in
  ( validate_common config ; validate_client config ; config )

let server
  ?ciphers ?version ?signature_algorithms ?reneg ?certificates ?acceptable_cas ?authenticator ?session_cache ?ticket_cache ?alpn_protocols ?groups ?zero_rtt () =
  let config =
    { default_config with
        ciphers = ciphers <?> default_config.ciphers ;
        protocol_versions = version <?> default_config.protocol_versions ;
        signature_algorithms = signature_algorithms <?> default_config.signature_algorithms ;
        use_reneg = reneg <?> default_config.use_reneg ;
        own_certificates = certificates <?> default_config.own_certificates ;
        acceptable_cas = acceptable_cas <?> default_config.acceptable_cas ;
        authenticator = authenticator ;
        session_cache = session_cache  <?> default_config.session_cache ;
        alpn_protocols = alpn_protocols <?> default_config.alpn_protocols ;
        ticket_cache = ticket_cache ;
        groups = groups <?> default_config.groups ;
        zero_rtt = zero_rtt <?> default_config.zero_rtt ;
    } in
  ( validate_common config ; validate_server config ; config )
