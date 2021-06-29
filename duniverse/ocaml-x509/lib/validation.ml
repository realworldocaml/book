open Rresult.R.Infix

let sha2 = [ `SHA256 ; `SHA384 ; `SHA512 ]
let all_hashes = [ `MD5 ; `SHA1 ; `SHA224 ] @ sha2

let src = Logs.Src.create "x509" ~doc:"X509 validation"
module Log = (val Logs.src_log src : Logs.LOG)

type signature_error = [
  | `Bad_signature of Distinguished_name.t * string
  | `Bad_encoding of Distinguished_name.t * string * Cstruct.t
  | `Hash_not_allowed of Distinguished_name.t * Mirage_crypto.Hash.hash
  | `Unsupported_keytype of Distinguished_name.t * Public_key.t
  | `Unsupported_algorithm of Distinguished_name.t * string
  | `Msg of string
]

let pp_signature_error ppf = function
  | `Bad_signature (subj, msg) ->
    Fmt.pf ppf "failed to verify signature of %a: %s"
      Distinguished_name.pp subj msg
  | `Bad_encoding (subj, err, sig_) ->
    Fmt.pf ppf "bad signature encoding of %a, ASN error %s:@.%a"
      Distinguished_name.pp subj err Cstruct.hexdump_pp sig_
  | `Hash_not_allowed (subj, hash) ->
    Fmt.pf ppf "hash algorithm %a is not allowed, but %a is signed using it"
      Distinguished_name.pp subj Certificate.pp_hash hash
  | `Unsupported_keytype (subj, pk) ->
    Fmt.pf ppf "unsupported key used to sign %a: %a" Distinguished_name.pp subj
      Public_key.pp pk
  | `Unsupported_algorithm (subj, alg) ->
    Fmt.pf ppf "unsupported algorithm used to sign %a: %s"
      Distinguished_name.pp subj alg
  | `Msg msg -> Fmt.string ppf msg

let maybe_validate_hostname cert = function
  | None   -> true
  | Some x -> Certificate.supports_hostname cert x

let issuer_matches_subject
    { Certificate.asn = parent ; _ } { Certificate.asn = cert ; _ } =
  Distinguished_name.equal parent.tbs_cert.subject cert.tbs_cert.issuer

let is_self_signed cert = issuer_matches_subject cert cert

let validate_raw_signature subject allowed_hashes msg sig_alg signature pk =
  match Algorithm.to_signature_algorithm sig_alg with
  | Some (scheme, siga) ->
    (* we check that siga is a member of allowed_hashes, to ensure not
       using a weak one. *)
    if not (List.mem siga allowed_hashes) then
      Error (`Hash_not_allowed (subject, siga))
    else if not (Key_type.supports_signature_scheme (Public_key.key_type pk) scheme) then
      Error (`Unsupported_keytype (subject, pk))
    else
      Rresult.R.reword_error
        (function `Msg m -> `Bad_signature (subject, m))
        (Public_key.verify siga ~scheme ~signature pk (`Message msg)) >>= fun () ->
      if not (List.mem siga sha2) then
        Log.warn (fun m -> m "%a signature uses %a, a weak hash algorithm"
                     Distinguished_name.pp subject Certificate.pp_hash siga);
      Ok ()
  | None ->
    Error (`Unsupported_algorithm (subject, Algorithm.to_string sig_alg))

(* XXX should return the tbs_cert blob from the parser, this is insane *)
let raw_cert_hack raw =
  (* we only support definite-length *)
  let loff = 1 in
  let snd = Cstruct.get_uint8 raw loff in
  let lenl = 2 + if 0x80 land snd = 0 then 0 else 0x7F land snd in
  (* cut away the SEQUENCE and LENGTH from outer sequence (tbs, sigalg, sig) *)
  let cert_buf = Cstruct.shift raw lenl in
  let rec l acc idx last =
    if idx = last then
      acc
    else
      l (acc lsl 8 + Cstruct.get_uint8 cert_buf idx) (succ idx) last
  in
  let cert_len_byte = Cstruct.get_uint8 cert_buf loff in
  let cert_len =
    (* two cases: *)
    if 0x80 land cert_len_byte = 0 then
      (* length < 127: highest bit is zero and lower 7 bits encode the length *)
      2 + (0x7F land cert_len_byte)
    else
      (* length > 127: highest bit is 1 and lower 7 bits encode the bytes used
         to encode the length *)
      let len_len = 2 + 0x7F land cert_len_byte in
      len_len + (l 0 2 len_len)
  in
  Cstruct.sub cert_buf 0 cert_len

let validate_signature allowed_hashes { Certificate.asn = trusted ; _ } { Certificate.asn ; raw } =
  let tbs_raw = raw_cert_hack raw in
  validate_raw_signature asn.tbs_cert.subject allowed_hashes tbs_raw
    asn.signature_algo asn.signature_val trusted.tbs_cert.pk_info

let validate_time time { Certificate.asn = cert ; _ } =
  match time with
  | None     -> true
  | Some now ->
    let (not_before, not_after) = cert.tbs_cert.validity in
    Ptime.(is_later ~than:not_before now && is_earlier ~than:not_after now)

let version_matches_extensions { Certificate.asn = cert ; _ } =
  let tbs = cert.tbs_cert in
  match tbs.version, Extension.is_empty tbs.extensions with
  | (`V1 | `V2), true -> true
  | (`V1 | `V2), _ -> false
  | `V3, _ -> true

let validate_path_len pathlen { Certificate.asn = cert ; _ } =
  (* X509 V1/V2 certificates do not contain X509v3 extensions! *)
  (* thus, we cannot check the path length. this will only ever happen for trust anchors: *)
  (* intermediate CAs are checked by is_cert_valid, which checks that the CA extensions are there *)
  (* whereas trust anchor are ok with getting V1/2 certificates *)
  (* TODO: make it configurable whether to accept V1/2 certificates at all *)
  let exts = cert.tbs_cert.extensions in
  match cert.tbs_cert.version, Extension.(find Basic_constraints exts) with
  | (`V1 | `V2), _ -> true
  | `V3, Some (_ , (true, None)) -> true
  | `V3, Some (_ , (true, Some n)) -> n >= pathlen
  | _ -> false

let validate_ca_extensions { Certificate.asn = cert ; _ } =
  let exts = cert.tbs_cert.extensions in
  (* comments from RFC5280 *)
  (* 4.2.1.9 Basic Constraints *)
  (* Conforming CAs MUST include this extension in all CA certificates used *)
  (* to validate digital signatures on certificates and MUST mark the *)
  (* extension as critical in such certificates *)
  (* unfortunately, there are 8 CA certs (including the one which
     signed google.com) which are _NOT_ marked as critical *)
  ( match Extension.(find Basic_constraints exts) with
    | Some (_ , (true, _)) -> true
    | _ -> false ) &&

  (* 4.2.1.3 Key Usage *)
  (* Conforming CAs MUST include key usage extension *)
  (* CA Cert (cacert.org) does not *)
  ( match Extension.(find Key_usage exts) with
    (* When present, conforming CAs SHOULD mark this extension as critical *)
    (* yeah, you wish... *)
    | Some (_, usage) -> List.mem `Key_cert_sign usage
    | _ -> false ) &&

  (* if we require this, we cannot talk to github.com
     (* 4.2.1.12.  Extended Key Usage
     If a certificate contains both a key usage extension and an extended
     key usage extension, then both extensions MUST be processed
     independently and the certificate MUST only be used for a purpose
     consistent with both extensions.  If there is no purpose consistent
     with both extensions, then the certificate MUST NOT be used for any
     purpose. *)
     ( match extn_ext_key_usage cert with
     | Some (_, Ext_key_usage usages) -> List.mem Any usages
     | _                              -> true ) &&
  *)

  (* Name Constraints - name constraints should match servername *)

  (* check criticality *)
  Extension.for_all (fun (Extension.B (k, v)) ->
      match k with
      | Extension.Key_usage -> true
      | Extension.Basic_constraints -> true
      | _ -> not (Extension.critical k v) )
    exts

let validate_server_extensions { Certificate.asn = cert ; _ } =
  Extension.for_all (fun (Extension.B (k, v)) ->
      match k, v with
      | Extension.Basic_constraints, (_, (true, _)) -> false
      | Extension.Basic_constraints, (_, (false, _)) -> true
      | Extension.Key_usage, _ -> true
      | Extension.Ext_key_usage, _ -> true
      | Extension.Subject_alt_name, _ -> true
      | Extension.Policies, (crit, ps) -> not crit || List.mem `Any ps
      (* we've to deal with _all_ extensions marked critical! *)
      | _, _ -> not (Extension.critical k v))
    cert.tbs_cert.extensions

let valid_trust_anchor_extensions cert =
  match cert.Certificate.asn.tbs_cert.version with
  | `V1 | `V2 -> true
  | `V3       -> validate_ca_extensions cert

let ext_authority_matches_subject trusted cert =
  match Extension.(find Authority_key_id (Certificate.extensions cert),
                   find Subject_key_id (Certificate.extensions trusted))
  with
  | (_, None) | (None, _)                       -> true (* not mandatory *)
  | Some (_, (Some auth, _, _)), Some (_, au)   -> Cstruct.equal auth au
  (* TODO: check exact rules in RFC5280 *)
  | Some (_, (None, _, _)), _                   -> true (* not mandatory *)

(* t -> t list (* set *) -> t list list *)
let rec build_paths fst rst =
  match
    List.filter
      (fun x -> Distinguished_name.equal (Certificate.issuer fst) (Certificate.subject x))
      rst
  with
  | [] -> [[fst]]
  | xs ->
    let tails =
      List.fold_left
        (fun acc x -> acc @ build_paths x (List.filter (fun y -> x <> y) rst))
        [[]]
        xs
    in
    List.map (fun x -> fst :: x) tails

type ca_error = [
  | signature_error
  | `CAIssuerSubjectMismatch of Certificate.t
  | `CAInvalidVersion of Certificate.t
  | `CACertificateExpired of Certificate.t * Ptime.t option
  | `CAInvalidExtensions of Certificate.t
]

let pp_ca_error ppf = function
  | #signature_error as e -> pp_signature_error ppf e
  | `CAIssuerSubjectMismatch c ->
    Fmt.pf ppf "CA certificate %a: issuer does not match subject" Certificate.pp c
  | `CAInvalidVersion c ->
    Fmt.pf ppf "CA certificate %a: version 3 is required for extensions" Certificate.pp c
  | `CAInvalidExtensions c ->
    Fmt.pf ppf "CA certificate %a: invalid CA extensions" Certificate.pp c
  | `CACertificateExpired (c, now) ->
    let pp_pt = Ptime.pp_human ~tz_offset_s:0 () in
    Fmt.pf ppf "CA certificate %a: expired (now %a)" Certificate.pp c
      Fmt.(option ~none:(unit "no timestamp provided") pp_pt) now

type leaf_validation_error = [
  | `LeafCertificateExpired of Certificate.t * Ptime.t option
  | `LeafInvalidName of Certificate.t * [`host] Domain_name.t option
  | `LeafInvalidVersion of Certificate.t
  | `LeafInvalidExtensions of Certificate.t
]

let pp_leaf_validation_error ppf = function
  | `LeafCertificateExpired (c, now) ->
    let pp_pt = Ptime.pp_human ~tz_offset_s:0 () in
    Fmt.pf ppf "leaf certificate %a expired (now %a)" Certificate.pp c
      Fmt.(option ~none:(unit "no timestamp provided") pp_pt) now
  | `LeafInvalidName (c, n) ->
    Fmt.pf ppf "leaf certificate %a does not contain the name %a"
      Certificate.pp c Fmt.(option ~none:(unit "none") Domain_name.pp) n
  | `LeafInvalidVersion c ->
    Fmt.pf ppf "leaf certificate %a: version 3 is required for extensions" Certificate.pp c
  | `LeafInvalidExtensions c ->
    Fmt.pf ppf "leaf certificate %a: invalid server extensions" Certificate.pp c

type chain_validation_error = [
  | `IntermediateInvalidExtensions of Certificate.t
  | `IntermediateCertificateExpired of Certificate.t * Ptime.t option
  | `IntermediateInvalidVersion of Certificate.t
  | `ChainIssuerSubjectMismatch of Certificate.t * Certificate.t
  | `ChainAuthorityKeyIdSubjectKeyIdMismatch of Certificate.t * Certificate.t
  | `ChainInvalidPathlen of Certificate.t * int
  | `EmptyCertificateChain
  | `NoTrustAnchor of Certificate.t
  | `Revoked of Certificate.t
]

let pp_chain_validation_error ppf = function
  | `IntermediateInvalidExtensions c ->
    Fmt.pf ppf "intermediate certificate %a: invalid extensions" Certificate.pp c
  | `IntermediateCertificateExpired (c, now) ->
    let pp_pt = Ptime.pp_human ~tz_offset_s:0 () in
    Fmt.pf ppf "intermediate certificate %a expired (now %a)" Certificate.pp c
      Fmt.(option ~none:(unit "no timestamp provided") pp_pt) now
  | `IntermediateInvalidVersion c ->
    Fmt.pf ppf "intermediate certificate %a: version 3 is required for extensions"
      Certificate.pp c
  | `ChainIssuerSubjectMismatch (c, parent) ->
    Fmt.pf ppf "invalid chain: issuer of %a does not match subject of %a"
      Certificate.pp c Certificate.pp parent
  | `ChainAuthorityKeyIdSubjectKeyIdMismatch (c, parent) ->
    Fmt.pf ppf "invalid chain: authority key id extension of %a does not match subject key id extension of %a"
      Certificate.pp c Certificate.pp parent
  | `ChainInvalidPathlen (c, pathlen) ->
    Fmt.pf ppf "invalid chain: the path length of %a is smaller than the required path length %d"
      Certificate.pp c pathlen
  | `EmptyCertificateChain -> Fmt.string ppf "certificate chain is empty"
  | `NoTrustAnchor c ->
    Fmt.pf ppf "no trust anchor found for %a" Certificate.pp c
  | `Revoked c ->
    Fmt.pf ppf "certificate %a is revoked" Certificate.pp c

type chain_error = [
  | signature_error
  | leaf_validation_error
  | chain_validation_error
]

let pp_chain_error ppf = function
  | #signature_error as e -> pp_signature_error ppf e
  | #leaf_validation_error as l -> pp_leaf_validation_error ppf l
  | #chain_validation_error as c -> pp_chain_validation_error ppf c

type fingerprint_validation_error = [
  | `ServerNameNotPresent of Certificate.t * [`host] Domain_name.t
  | `NameNotInList of Certificate.t
  | `InvalidFingerprint of Certificate.t * Cstruct.t * Cstruct.t
]

let pp_fingerprint_validation_error ppf = function
  | `ServerNameNotPresent (c, n) ->
    Fmt.pf ppf "server name %a was matched in the fingerprint list, but does not occur in certificate %a"
      Domain_name.pp n Certificate.pp c
  | `NameNotInList c ->
    Fmt.pf ppf "certificate common name %a is not present in the fingerprint list"
      Certificate.pp c
  | `InvalidFingerprint (c, c_fp, fp) ->
    Fmt.pf ppf "fingerprint for %a (which is %a) does not match, expected %a"
      Certificate.pp c Cstruct.hexdump_pp c_fp Cstruct.hexdump_pp fp

type validation_error = [
  | signature_error
  | leaf_validation_error
  | fingerprint_validation_error
  | `EmptyCertificateChain
  | `InvalidChain
]

let pp_validation_error ppf = function
  | #signature_error as e -> pp_signature_error ppf e
  | #leaf_validation_error as l -> pp_leaf_validation_error ppf l
  | #fingerprint_validation_error as f -> pp_fingerprint_validation_error ppf f
  | `EmptyCertificateChain ->
    Fmt.string ppf "provided certificate chain is empty"
  | `InvalidChain -> Fmt.string ppf "invalid certificate chain"

type r = ((Certificate.t list * Certificate.t) option, validation_error) result

(* TODO RFC 5280: A certificate MUST NOT include more than one
   instance of a particular extension. *)

let is_cert_valid now cert =
  match
    validate_time now cert,
    version_matches_extensions cert,
    validate_ca_extensions cert
  with
  | (true, true, true) -> Ok ()
  | (false, _, _)      -> Error (`IntermediateCertificateExpired (cert, now))
  | (_, false, _)      -> Error (`IntermediateInvalidVersion cert)
  | (_, _, false)      -> Error (`IntermediateInvalidExtensions cert)

let is_ca_cert_valid allowed_hashes now cert =
  match
    is_self_signed cert,
    version_matches_extensions cert,
    validate_signature allowed_hashes cert cert,
    validate_time now cert,
    valid_trust_anchor_extensions cert
  with
  | (true, true, Ok (), true, true) -> Ok ()
  | (false, _, _, _, _)            -> Error (`CAIssuerSubjectMismatch cert)
  | (_, false, _, _, _)            -> Error (`CAInvalidVersion cert)
  | (_, _, Error e, _, _)          -> Error e
  | (_, _, _, false, _)            -> Error (`CACertificateExpired (cert, now))
  | (_, _, _, _, false)            -> Error (`CAInvalidExtensions cert)

let valid_ca ?(allowed_hashes = all_hashes) ?time cacert =
  is_ca_cert_valid allowed_hashes time cacert

let is_server_cert_valid host now cert =
  match
    validate_time now cert,
    maybe_validate_hostname cert host,
    version_matches_extensions cert,
    validate_server_extensions cert
  with
  | (true, true, true, true) -> Ok ()
  | (false, _, _, _)         -> Error (`LeafCertificateExpired (cert, now))
  | (_, false, _, _)         -> Error (`LeafInvalidName (cert, host))
  | (_, _, false, _)         -> Error (`LeafInvalidVersion cert)
  | (_, _, _, false)         -> Error (`LeafInvalidExtensions cert)

let signs hash pathlen trusted cert =
  match
    issuer_matches_subject trusted cert,
    ext_authority_matches_subject trusted cert,
    validate_signature hash trusted cert,
    validate_path_len pathlen trusted
  with
  | (true, true, Ok (), true) -> Ok ()
  | (false, _, _, _)         -> Error (`ChainIssuerSubjectMismatch (trusted, cert))
  | (_, false, _, _)         -> Error (`ChainAuthorityKeyIdSubjectKeyIdMismatch (trusted, cert))
  | (_, _, Error e, _)       -> Error e
  | (_, _, _, false)         -> Error (`ChainInvalidPathlen (trusted, pathlen))

let issuer trusted cert =
  List.filter (fun p -> issuer_matches_subject p cert) trusted

let rec validate_anchors revoked hash pathlen cert = function
  | []    -> Error (`NoTrustAnchor cert)
  | x::xs -> match signs hash pathlen x cert with
    | Ok _    -> if revoked ~issuer:x ~cert then Error (`Revoked cert) else Ok x
    | Error _ -> validate_anchors revoked hash pathlen cert xs

let verify_single_chain now ?(revoked = fun ~issuer:_ ~cert:_ -> false) hash anchors chain =
  let rec climb pathlen = function
    | cert :: issuer :: certs ->
      is_cert_valid now issuer >>= fun () ->
      (if revoked ~issuer ~cert then Error (`Revoked cert) else Ok ()) >>= fun () ->
      signs hash pathlen issuer cert >>= fun () ->
      climb (succ pathlen) (issuer :: certs)
    | [c] ->
      let anchors = issuer anchors c in
      validate_anchors revoked hash pathlen c anchors
    | [] -> Error `EmptyCertificateChain
  in
  climb 0 chain

let verify_chain ~host ~time ?revoked ?(allowed_hashes = sha2) ~anchors = function
  | [] -> Error `EmptyCertificateChain
  | server :: certs ->
    let now = time () in
    let anchors = List.filter (validate_time now) anchors in
    is_server_cert_valid host now server >>= fun () ->
    verify_single_chain now ?revoked allowed_hashes anchors (server :: certs)

let rec any_m e f = function
  | [] -> Error e
  | c::cs -> match f c with
    | Ok ta -> Ok (Some (c, ta))
    | Error _ -> any_m e f cs

let verify_chain_of_trust ~host ~time ?revoked ?(allowed_hashes = sha2) ~anchors = function
  | [] -> Error `EmptyCertificateChain
  | server :: certs ->
    let now = time () in
    (* verify server! *)
    is_server_cert_valid host now server >>= fun () ->
    (* build all paths *)
    let paths = build_paths server certs
    and anchors = List.filter (validate_time now) anchors
    in
    (* exists there one which is good? *)
    any_m `InvalidChain (verify_single_chain now ?revoked allowed_hashes anchors) paths

let valid_cas ?(allowed_hashes = all_hashes) ?time cas =
  List.filter (fun cert ->
      Rresult.R.is_ok (is_ca_cert_valid allowed_hashes time cert))
    cas

let fingerprint_verification host now fingerprints fp = function
  | [] -> Error `EmptyCertificateChain
  | server::_ ->
    let verify_fingerprint server fingerprints =
      let fingerprint = fp server in
      let fp_matches (_, fp') = Cstruct.equal fp' fingerprint in
      if List.exists fp_matches fingerprints then
        let name, _ = List.find fp_matches fingerprints in
        if maybe_validate_hostname server (Some name) then
          Ok None
        else
          Error (`ServerNameNotPresent (server, name))
      else
        let name_matches (n, _) = Certificate.supports_hostname server n in
        if List.exists name_matches fingerprints then
          let (_, fp) = List.find name_matches fingerprints in
          Error (`InvalidFingerprint (server, fingerprint, fp))
        else
          Error (`NameNotInList server)
    in
    match validate_time now server, maybe_validate_hostname server host with
    | true , true  -> verify_fingerprint server fingerprints
    | false, _     -> Error (`LeafCertificateExpired (server, now))
    | _    , false -> Error (`LeafInvalidName (server, host))

let trust_key_fingerprint ~host ~time ~hash ~fingerprints =
  let now = time () in
  let fp cert = Public_key.fingerprint ~hash (Certificate.public_key cert) in
  fingerprint_verification host now fingerprints fp

let trust_cert_fingerprint ~host ~time ~hash ~fingerprints =
  let now = time () in
  let fp = Certificate.fingerprint hash in
  fingerprint_verification host now fingerprints fp

(* RFC5246 says 'root certificate authority MAY be omitted' *)

(* TODO: how to deal with
    2.16.840.1.113730.1.1 - Netscape certificate type
    2.16.840.1.113730.1.12 - SSL server name
    2.16.840.1.113730.1.13 - Netscape certificate comment *)

(* stuff from 4366 (TLS extensions):
  - root CAs
  - client cert url *)

(* Future TODO Certificate Revocation Lists and OCSP (RFC6520)
2.16.840.1.113730.1.2 - Base URL
2.16.840.1.113730.1.3 - Revocation URL
2.16.840.1.113730.1.4 - CA Revocation URL
2.16.840.1.113730.1.7 - Renewal URL
2.16.840.1.113730.1.8 - Netscape CA policy URL

2.5.4.38 - id-at-authorityRevocationList
2.5.4.39 - id-at-certificateRevocationList

do not forget about 'authority information access' (private internet extension -- 4.2.2 of 5280) *)

(* Future TODO: Policies
2.5.29.32 - Certificate Policies
2.5.29.33 - Policy Mappings
2.5.29.36 - Policy Constraints
 *)

(* Future TODO: anything with subject_id and issuer_id ? seems to be not used by anybody *)
