open Nocrypto

open Utils
open Core

type certchain = X509.Certificate.t list * Rsa.priv

type own_cert = [
  | `None
  | `Single of certchain
  | `Multiple of certchain list
  | `Multiple_default of certchain * certchain list
]

type session_cache = SessionID.t -> epoch_data option

let config_of_sexp _ = failwith "not implemented"
let sexp_of_config _ = Sexplib.Sexp.Atom "CONFIG"
let server_of_sexp _ = failwith "not implemented"
let sexp_of_server _ = Sexplib.Sexp.Atom "CONFIG"
let client_of_sexp _ = failwith "not implemented"
let sexp_of_client _ = Sexplib.Sexp.Atom "CONFIG"

type config = {
  ciphers           : Ciphersuite.ciphersuite list ;
  protocol_versions : tls_version * tls_version ;
  hashes            : Hash.hash list ;
  (* signatures        : Packet.signature_algorithm_type list ; *)
  use_reneg         : bool ;
  authenticator     : X509.Authenticator.t option ;
  wildcard_enabled  : bool ;
  peer_name         : [`host] Domain_name.t option ;
  own_certificates  : own_cert ;
  acceptable_cas    : X509.Distinguished_name.t list ;
  session_cache     : session_cache ;
  cached_session    : epoch_data option ;
  alpn_protocols    : string list ;
}

module Ciphers = struct

  (* A good place for various pre-baked cipher lists and helper functions to
   * slice and groom those lists. *)

  let default = [
    `TLS_DHE_RSA_WITH_AES_256_CCM ;
    `TLS_DHE_RSA_WITH_AES_128_CCM ;
    `TLS_DHE_RSA_WITH_AES_256_CBC_SHA256 ;
    `TLS_DHE_RSA_WITH_AES_128_CBC_SHA256 ;
    `TLS_DHE_RSA_WITH_AES_256_CBC_SHA ;
    `TLS_DHE_RSA_WITH_AES_128_CBC_SHA ;
    `TLS_RSA_WITH_AES_256_CCM ;
    `TLS_RSA_WITH_AES_128_CCM ;
    `TLS_RSA_WITH_AES_256_CBC_SHA256 ;
    `TLS_RSA_WITH_AES_128_CBC_SHA256 ;
    `TLS_RSA_WITH_AES_256_CBC_SHA ;
    `TLS_RSA_WITH_AES_128_CBC_SHA ;
    ]

  let supported = default @ [
    `TLS_DHE_RSA_WITH_AES_256_GCM_SHA384 ;
    `TLS_DHE_RSA_WITH_AES_128_GCM_SHA256 ;
    `TLS_RSA_WITH_AES_256_GCM_SHA384 ;
    `TLS_RSA_WITH_AES_128_GCM_SHA256 ;
    `TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA ;
    `TLS_RSA_WITH_3DES_EDE_CBC_SHA ;
    `TLS_RSA_WITH_RC4_128_SHA ;
    `TLS_RSA_WITH_RC4_128_MD5
    ]

  let fs_of = List.filter Ciphersuite.ciphersuite_fs

  let fs = fs_of default

end

let default_hashes =
  [ `SHA512 ; `SHA384 ; `SHA256 ; `SHA224 ; `SHA1 ]

let supported_hashes =
  default_hashes @ [ `MD5 ]

let min_dh_size = 1024

let min_rsa_key_size = 1024

let dh_group = Dh.Group.ffdhe2048 (* ff-dhe draft 2048-bit group *)

let default_config = {
  ciphers           = Ciphers.default ;
  protocol_versions = (TLS_1_0, TLS_1_2) ;
  hashes            = default_hashes ;
  use_reneg         = false ;
  authenticator     = None ;
  wildcard_enabled  = true ;
  peer_name         = None ;
  own_certificates  = `None ;
  acceptable_cas    = [] ;
  session_cache     = (fun _ -> None) ;
  cached_session    = None ;
  alpn_protocols    = [] ;
}

let invalid msg = invalid_arg ("Tls.Config: invalid configuration: " ^ msg)

let validate_common config =
  let (v_min, v_max) = config.protocol_versions in
  if v_max < v_min then invalid "bad version range" ;
  ( match config.hashes with
    | [] when v_max >= TLS_1_2                          ->
       invalid "TLS 1.2 configured but no hashes provided"
    | hs when not (List_set.subset hs supported_hashes) ->
       invalid "Some hash algorithms are not supported"
    | _                                                 ->
       () ) ;
  if not (List_set.is_proper_set config.ciphers) then
    invalid "set of ciphers is not a proper set" ;
  if List.length config.ciphers = 0 then
    invalid "set of ciphers is empty" ;
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
     let pub = Rsa.pub_of_priv priv in
     if Rsa.pub_bits pub < min_rsa_key_size then
       invalid "RSA key too short!" ;
     ( match X509.Certificate.public_key s with
       | `RSA pub' when pub = pub' -> ()
       | _ -> invalid "public / private key combination" ) ;
     ( match init_and_last chain with
       | Some (ch, trust) ->
         (* TODO: verify that certificates are x509 v3 if TLS_1_2 *)
         ( match X509.Validation.verify_chain_of_trust ~anchors:[trust] (s :: ch) with
           | Ok _ -> ()
           | Error x -> invalid ("certificate chain does not validate: " ^
                                 Fmt.to_to_string X509.Validation.pp_validation_error x) )
       | None -> () )
  | _ -> invalid "certificate"

let validate_client config =
  match config.own_certificates with
  | `None -> ()
  | `Single c -> validate_certificate_chain c
  | _ -> invalid_arg "multiple client certificates not supported in client config"

module StringSet = Set.Make(String)

let non_overlapping cs =
  let namessets =
    filter_map cs ~f:(function
        | (s :: _, _) -> Some s
        | _           -> None)
    |> List.map X509.Certificate.hostnames
  in
  let rec check = function
    | []    -> ()
    | s::ss -> if not (List.for_all
                         (fun ss' -> Domain_name.Set.is_empty (Domain_name.Set.inter s ss'))
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
        List.filter needs_certificate |>
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
    let open X509 in
    not (CertTypeUsageSet.for_all
           (fun (t, u) ->
              List.exists (fun c ->
                  Certificate.supports_keytype c t &&
                  (match Extension.(find Key_usage (Certificate.extensions c)) with
                   | None -> true
                   | Some (_, ku) -> List.mem u ku))
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


type client = config
type server = config

let of_server conf = conf
and of_client conf = conf

let peer conf name = { conf with peer_name = Some name }

let with_authenticator conf auth = { conf with authenticator = Some auth }

let with_own_certificates conf own_certificates = { conf with own_certificates }

let with_acceptable_cas conf acceptable_cas = { conf with acceptable_cas }

let (<?>) ma b = match ma with None -> b | Some a -> a

let client
  ~authenticator ?(wildcard_enabled = true) ?peer_name ?ciphers ?version ?hashes ?reneg ?certificates ?cached_session ?alpn_protocols () =
  let config =
    { default_config with
        authenticator     = Some authenticator ;
        ciphers           = ciphers        <?> default_config.ciphers ;
        protocol_versions = version        <?> default_config.protocol_versions ;
        hashes            = hashes         <?> default_config.hashes ;
        use_reneg         = reneg          <?> default_config.use_reneg ;
        own_certificates  = certificates   <?> default_config.own_certificates ;
        wildcard_enabled ;
        peer_name         = peer_name ;
        cached_session    = cached_session ;
        alpn_protocols    = alpn_protocols <?> default_config.alpn_protocols ;
    } in
  ( validate_common config ; validate_client config ; config )

let server
  ?ciphers ?version ?hashes ?reneg ?certificates ?acceptable_cas ?authenticator ?session_cache ?alpn_protocols () =
  let config =
    { default_config with
        ciphers           = ciphers        <?> default_config.ciphers ;
        protocol_versions = version        <?> default_config.protocol_versions ;
        hashes            = hashes         <?> default_config.hashes ;
        use_reneg         = reneg          <?> default_config.use_reneg ;
        own_certificates  = certificates   <?> default_config.own_certificates ;
        acceptable_cas    = acceptable_cas <?> default_config.acceptable_cas ;
        authenticator     = authenticator ;
        session_cache     = session_cache  <?> default_config.session_cache ;
        alpn_protocols    = alpn_protocols <?> default_config.alpn_protocols ;
    } in
  ( validate_common config ; validate_server config ; config )
