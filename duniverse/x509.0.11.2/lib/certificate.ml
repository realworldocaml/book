type key_type = [ `RSA | `EC of Asn.oid ]

(*
 * X509 certs
 *)
type tBSCertificate = {
  version    : [ `V1 | `V2 | `V3 ] ;
  serial     : Z.t ;
  signature  : Algorithm.t ;
  issuer     : Distinguished_name.t ;
  validity   : Ptime.t * Ptime.t ;
  subject    : Distinguished_name.t ;
  pk_info    : Public_key.t ;
  issuer_id  : Cstruct.t option ;
  subject_id : Cstruct.t option ;
  extensions : Extension.t
}

type certificate = {
  tbs_cert       : tBSCertificate ;
  signature_algo : Algorithm.t ;
  signature_val  : Cstruct.t
}

(*
 * There are two reasons to carry Cstruct.t around:
 * - we still need to hack on the cstruct to get bytes to hash
 *   ( this needs to go )
 * - we need a cs to send to the peer
 * It's a bit ugly to have two levels, and both are better solved by extending
 * the asn parser and writer respectively, but until then there needs to be one
 * place that hides the existence of this pair.
 *)
type t = {
  asn : certificate ;
  raw : Cstruct.t
}

module Asn = struct
  open Asn.S
  open Asn_grammars

  let version =
    map (function 2 -> `V3 | 1 -> `V2 | 0 -> `V1 | _ -> parse_error "unknown version")
      (function `V3 -> 2 | `V2 -> 1 | `V1 -> 0)
      int

  let certificate_sn = integer

  let time =
    let f = function `C1 t -> t | `C2 t -> t
    and g t =
      let (y, _, _) = Ptime.to_date t in
      if y < 2050 then `C1 t else `C2 t in
    map f g (choice2 utc_time generalized_time_no_frac_s)

  let validity =
    sequence2
      (required ~label:"not before" time)
      (required ~label:"not after"  time)

  let unique_identifier = bit_string_cs

  let tBSCertificate =
    let f = fun (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) ->
      let extn = match j with None -> Extension.empty | Some xs -> xs
      in
      { version    = def `V1 a ; serial     = b ;
        signature  = c         ; issuer     = d ;
        validity   = e         ; subject    = f ;
        pk_info    = g         ; issuer_id  = h ;
        subject_id = i         ; extensions = extn }
    and g = fun
      { version    = a ; serial     = b ;
        signature  = c ; issuer     = d ;
        validity   = e ; subject    = f ;
        pk_info    = g ; issuer_id  = h ;
        subject_id = i ; extensions = j } ->
      let extn = if Extension.is_empty j then None else Some j
      in
      (def' `V1 a, (b, (c, (d, (e, (f, (g, (h, (i, extn)))))))))
    in
    map f g @@
    sequence @@
    (optional ~label:"version"       @@ explicit 0 version) (* default v1 *)
    @ (required ~label:"serialNumber"  @@ certificate_sn)
    @ (required ~label:"signature"     @@ Algorithm.identifier)
    @ (required ~label:"issuer"        @@ Distinguished_name.Asn.name)
    @ (required ~label:"validity"      @@ validity)
    @ (required ~label:"subject"       @@ Distinguished_name.Asn.name)
    @ (required ~label:"subjectPKInfo" @@ Public_key.Asn.pk_info_der)
      (* if present, version is v2 or v3 *)
    @ (optional ~label:"issuerUID"     @@ implicit 1 unique_identifier)
      (* if present, version is v2 or v3 *)
    @ (optional ~label:"subjectUID"    @@ implicit 2 unique_identifier)
      (* v3 if present *)
   -@ (optional ~label:"extensions"    @@ explicit 3 Extension.Asn.extensions_der)

  let (tbs_certificate_of_cstruct, tbs_certificate_to_cstruct) =
    projections_of Asn.der tBSCertificate

  let certificate =
    let f (a, b, c) =
      if a.signature <> b then
        parse_error "signatureAlgorithm != tbsCertificate.signature"
      else
        { tbs_cert = a; signature_algo = b; signature_val = c }
    and g { tbs_cert = a; signature_algo = b; signature_val = c } = (a, b, c) in
    map f g @@
    sequence3
      (required ~label:"tbsCertificate"     tBSCertificate)
      (required ~label:"signatureAlgorithm" Algorithm.identifier)
      (required ~label:"signatureValue"     bit_string_cs)

  let (certificate_of_cstruct, certificate_to_cstruct) =
    projections_of Asn.der certificate

  let pkcs1_digest_info =
    let open Algorithm in
    let f (algo, cs) =
      match to_hash algo with
      | Some h -> (h, cs)
      | None   -> parse_error "pkcs1 digest info: unknown hash"
    and g (h, cs) = (of_hash h, cs)
    in
    map f g @@
    sequence2
      (required ~label:"digestAlgorithm" Algorithm.identifier)
      (required ~label:"digest"          octet_string)

  let (pkcs1_digest_info_of_cstruct, pkcs1_digest_info_to_cstruct) =
    projections_of Asn.der pkcs1_digest_info
end

let decode_pkcs1_digest_info cs =
  Asn_grammars.err_to_msg (Asn.pkcs1_digest_info_of_cstruct cs)

let encode_pkcs1_digest_info = Asn.pkcs1_digest_info_to_cstruct

let decode_der cs =
  let open Rresult.R.Infix in
  Asn_grammars.err_to_msg (Asn.certificate_of_cstruct cs) >>| fun asn ->
  { asn ; raw = cs }

let encode_der { raw ; _ } = raw

let decode_pem_multiple cs =
  let open Rresult.R.Infix in
  Pem.parse cs >>= fun data ->
  let certs = List.filter (fun (t, _) -> String.equal "CERTIFICATE" t) data in
  Pem.foldM (fun (_, cs) -> decode_der cs) certs

let decode_pem cs =
  let open Rresult.R.Infix in
  decode_pem_multiple cs >>= Pem.exactly_one ~what:"certificate"

let encode_pem v =
  Pem.unparse ~tag:"CERTIFICATE" (encode_der v)

let encode_pem_multiple cs =
  Cstruct.concat (List.map encode_pem cs)

let pp_version ppf v =
  Fmt.string ppf (match v with `V1 -> "1" | `V2 -> "2" | `V3 -> "3")

let pp_hash ppf hash =
  Fmt.string ppf (match hash with
      | `MD5 -> "MD5" | `SHA1 -> "SHA1" | `SHA224 -> "SHA224"
      | `SHA256 -> "SHA256" | `SHA384 -> "SHA384" | `SHA512 -> "SHA512")

let pp_sigalg ppf (asym, hash) =
  Fmt.pf ppf "%s-%a"
    (match asym with `RSA -> "RSA" | `ECDSA -> "ECDSA")
    pp_hash hash

let pp ppf { asn ; _ } =
  let tbs = asn.tbs_cert in
  let sigalg = Algorithm.to_signature_algorithm tbs.signature in
  Fmt.pf ppf "X.509 certificate@.version %a@.serial %a@.algorithm %a@.issuer %a@.valid from %a until %a@.subject %a@.extensions %a"
    pp_version tbs.version Z.pp_print tbs.serial
    Fmt.(option ~none:(unit "NONE") pp_sigalg) sigalg
    Distinguished_name.pp tbs.issuer
    (Ptime.pp_human ~tz_offset_s:0 ()) (fst tbs.validity)
    (Ptime.pp_human ~tz_offset_s:0 ()) (snd tbs.validity)
    Distinguished_name.pp tbs.subject
    Extension.pp tbs.extensions

let fingerprint hash cert = Mirage_crypto.Hash.digest hash cert.raw

let issuer { asn ; _ } = asn.tbs_cert.issuer

let subject { asn ; _ } = asn.tbs_cert.subject

let serial { asn ; _ } = asn.tbs_cert.serial

let validity { asn ; _ } = asn.tbs_cert.validity

let signature_algorithm { asn ; _ } =
  Algorithm.to_signature_algorithm asn.signature_algo

let public_key { asn = cert ; _ } = cert.tbs_cert.pk_info

let supports_keytype c t =
  match public_key c, t with
  | (`RSA _), `RSA -> true
  | _              -> false

let extensions { asn = cert ; _ } = cert.tbs_cert.extensions

(* RFC 6125, 6.4.4:
   Therefore, if and only if the presented identifiers do not include a
   DNS-ID, SRV-ID, URI-ID, or any application-specific identifier types
   supported by the client, then the client MAY as a last resort check
   for a string whose form matches that of a fully qualified DNS domain
   name in a Common Name field of the subject field (i.e., a CN-ID).  If
   the client chooses to compare a reference identifier of type CN-ID
   against that string, it MUST follow the comparison rules for the DNS
   domain name portion of an identifier of type DNS-ID, SRV-ID, or
   URI-ID, as described under Section 6.4.1, Section 6.4.2, and
   Section 6.4.3. *)
let hostnames { asn = cert ; _ } =
  let subj =
    match Distinguished_name.common_name cert.tbs_cert.subject with
    | None -> Host.Set.empty
    | Some x ->
      match Host.host x with
      | Some (wild, d) -> Host.Set.singleton (wild, d)
      | None -> Host.Set.empty
  in
  match Extension.hostnames cert.tbs_cert.extensions with
  | Some names -> names
  | None -> subj

let supports_hostname cert name =
  let names = hostnames cert in
  let wc_name_opt =
    match Domain_name.drop_label name with
    | Error _ -> None
    | Ok name -> match Domain_name.host name with
      | Ok hostname -> Some hostname
      | Error _ -> None
  in
  Host.Set.mem (`Strict, name) names
  || (match wc_name_opt with
      | None -> false
      | Some wc_name -> Host.Set.mem (`Wildcard, wc_name) names)
