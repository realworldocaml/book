open Rresult

type revoked_cert = {
  serial : Z.t ;
  date : Ptime.t ;
  extensions : Extension.t
}

type tBS_CRL = {
  version : [ `V1 | `V2 ] ;
  signature : Algorithm.t ;
  issuer : Distinguished_name.t ;
  this_update : Ptime.t ;
  next_update : Ptime.t option ;
  revoked_certs : revoked_cert list ;
  extensions : Extension.t
}

type crl = {
  tbs_crl : tBS_CRL ;
  signature_algo : Algorithm.t ;
  signature_val : Cstruct.t
}

module Asn = struct
  open Asn.S
  open Asn_grammars

  let revokedCertificate =
    let f (serial, date, e) =
      let extensions = match e with None -> Extension.empty | Some xs -> xs in
      { serial ; date ; extensions }
    and g { serial ; date ; extensions } =
      let e = if Extension.is_empty extensions then None else Some extensions in
      (serial, date, e)
    in
    map f g @@
    sequence3
      (required ~label:"userCertificate" @@ Certificate.Asn.certificate_sn)
      (required ~label:"revocationDate" @@ Certificate.Asn.time)
      (optional ~label:"crlEntryExtensions" @@ Extension.Asn.extensions_der)

  let version =
    map
      (function 0 -> `V1 | 1 -> `V2 | _ -> parse_error "unknown version")
      (function `V2 -> 1 | `V1 -> 0)
      int

  let tBSCertList =
    let f (a, (b, (c, (d, (e, (f, g)))))) =
      { version = def `V1 a ; signature = b ; issuer = c ;
        this_update = d ; next_update = e ;
        revoked_certs = (match f with None -> [] | Some xs -> xs) ;
        extensions = (match g with None -> Extension.empty | Some xs -> xs) }
    and g { version = a ; signature = b ; issuer = c ;
            this_update = d ; next_update = e ; revoked_certs = f ;
            extensions = g } =
      let f = match f with [] -> None | xs -> Some xs
      and g = if Extension.is_empty g then None else Some g
      in
      (def' `V1 a, (b, (c, (d, (e, (f, g))))))
    in
    map f g @@
    sequence @@
    (optional ~label:"version" @@ version)
    @ (required ~label:"signature" @@ Algorithm.identifier)
    @ (required ~label:"issuer" @@ Distinguished_name.Asn.name)
    @ (required ~label:"thisUpdate" @@ Certificate.Asn.time)
    @ (optional ~label:"nextUpdate" @@ Certificate.Asn.time)
    @ (optional ~label:"revokedCertificates" @@ sequence_of revokedCertificate)
      -@ (optional ~label:"crlExtensions" @@ explicit 0 Extension.Asn.extensions_der)

  let certificateList =
    let f (cl, sa, sv) =
      if cl.signature <> sa then
        parse_error "signatureAlgorithm != tbsCertList.signature"
      else
        { tbs_crl = cl ; signature_algo = sa ; signature_val = sv }
    and g { tbs_crl ; signature_algo ; signature_val } =
      (tbs_crl, signature_algo, signature_val)
    in
    map f g @@
    sequence3
      (required ~label:"tbsCertList" @@ tBSCertList)
      (required ~label:"signatureAlgorithm" @@ Algorithm.identifier)
      (required ~label:"signatureValue" @@ bit_string_cs)

  let (crl_of_cstruct, crl_to_cstruct) =
    projections_of Asn.der certificateList

  let (tbs_CRL_of_cstruct, tbs_CRL_to_cstruct) =
    projections_of Asn.der tBSCertList
end

type t = {
  raw : Cstruct.t ;
  asn : crl ;
}

let guard p e = if p then Ok () else Error e

let decode_der raw =
  Asn_grammars.err_to_msg (Asn.crl_of_cstruct raw) >>| fun asn ->
  { raw ; asn }

let encode_der { raw ; _ } = raw

let issuer { asn ; _ } = asn.tbs_crl.issuer

let this_update { asn ; _ } = asn.tbs_crl.this_update

let next_update { asn ; _ } = asn.tbs_crl.next_update

let extensions { asn ; _ } = asn.tbs_crl.extensions

let revoked_certificates { asn ; _ } = asn.tbs_crl.revoked_certs

let crl_number { asn ; _ } =
  match Extension.(find CRL_number asn.tbs_crl.extensions) with
  | None -> None
  | Some (_, x) -> Some x

let signature_algorithm { asn ; _ } =
  Algorithm.to_signature_algorithm asn.signature_algo

let validate { raw ; asn } ?(hash_whitelist = Validation.sha2) pub =
  let tbs_raw = Validation.raw_cert_hack raw asn.signature_val in
  Validation.validate_raw_signature asn.tbs_crl.issuer hash_whitelist
    tbs_raw asn.signature_algo asn.signature_val pub

type verification_error = [
  | Validation.signature_error
  | `Issuer_subject_mismatch of Distinguished_name.t * Distinguished_name.t
  | `Not_yet_valid of Distinguished_name.t * Ptime.t * Ptime.t
  | `Next_update_scheduled of Distinguished_name.t * Ptime.t * Ptime.t
]

let pp_verification_error ppf = function
  | #Validation.signature_error as e -> Validation.pp_signature_error ppf e
  | `Issuer_subject_mismatch (issuer, subj) ->
    Fmt.pf ppf "issuer %a does not match subject %a"
      Distinguished_name.pp issuer Distinguished_name.pp subj
  | `Not_yet_valid (issuer, now, created) ->
    Fmt.pf ppf "CRL %a not yet valid, valid from %a, now %a"
      Distinguished_name.pp issuer
      (Ptime.pp_human ~tz_offset_s:0 ()) created
      (Ptime.pp_human ~tz_offset_s:0 ()) now
  | `Next_update_scheduled (issuer, now, scheduled) ->
    Fmt.pf ppf "CRL %a next update already scheduled at %a, now %a"
      Distinguished_name.pp issuer
      (Ptime.pp_human ~tz_offset_s:0 ()) scheduled
      (Ptime.pp_human ~tz_offset_s:0 ()) now

let verify ({ asn ; _ } as crl) ?hash_whitelist ?time cert =
  let open Rresult.R.Infix in
  let subj = Certificate.subject cert in
  guard
    (Distinguished_name.equal asn.tbs_crl.issuer subj)
    (`Issuer_subject_mismatch (asn.tbs_crl.issuer, subj)) >>= fun () ->
  (match time with
   | None -> Ok ()
   | Some x ->
     guard (Ptime.is_later ~than:asn.tbs_crl.this_update x)
       (`Not_yet_valid (subj, x, asn.tbs_crl.this_update)) >>= fun () ->
     match asn.tbs_crl.next_update with
     | None -> Ok ()
     | Some y -> guard (Ptime.is_earlier ~than:y x)
                   (`Next_update_scheduled (subj, x, y))) >>= fun () ->
  validate ?hash_whitelist crl (Certificate.public_key cert)

let reason (revoked : revoked_cert) =
  match Extension.(find Reason revoked.extensions) with
  | Some (_, x) -> Some x
  | None -> None

let is_revoked (crls : t list) ?hash_whitelist ~issuer:super ~cert =
  List.exists (fun crl ->
      if
        Distinguished_name.equal (Certificate.subject super) (issuer crl)
      then
        match validate ?hash_whitelist crl (Certificate.public_key super) with
        | Ok () ->
          begin try
              let entry = List.find
                  (fun r -> Z.equal (Certificate.serial cert) r.serial)
                  (revoked_certificates crl)
              in
              match reason entry with
              | None -> true
              | Some `Remove_from_CRL -> false
              | Some _ -> true
            with Not_found -> false
          end
        | Error _ -> false
      else
        false)
    crls

let sign_tbs (tbs : tBS_CRL) key =
  let tbs_raw = Asn.tbs_CRL_to_cstruct tbs in
  let digest = match Algorithm.to_signature_algorithm tbs.signature with
    | Some (_, h) -> h
    | _ -> invalid_arg "couldn't parse signature algorithm"
  in
  let signature_val = Signing_request.raw_sign tbs_raw digest key in
  let asn = { tbs_crl = tbs ; signature_algo = tbs.signature ; signature_val } in
  let raw = Asn.crl_to_cstruct asn in
  { asn ; raw }

let revoke
    ?(digest = `SHA256)
    ~issuer
    ~this_update ?next_update
    ?(extensions = Extension.empty)
    revoked_certs
    key =
  let signature =
    Algorithm.of_signature_algorithm (Private_key.keytype key) digest
  in
  let tbs_crl = {
    version = `V2 ;
    signature ;
    issuer ;
    this_update ; next_update ;
    revoked_certs ;
    extensions
  }
  in
  sign_tbs tbs_crl key

let revoke_certificates (revoked : revoked_cert list) ~this_update ?next_update ({ asn ; _ } as crl) key =
  let tbs = asn.tbs_crl in
  let count = match crl_number crl with None -> 0 | Some x -> succ x in
  let extensions = Extension.(add CRL_number (false, count) tbs.extensions) in
  let tbs = {
    tbs with revoked_certs = tbs.revoked_certs @ revoked ;
             this_update ; next_update ;
             extensions
  }
  in
  sign_tbs tbs key

let revoke_certificate revoked ~this_update ?next_update crl key =
  revoke_certificates [revoked] ~this_update ?next_update crl key
