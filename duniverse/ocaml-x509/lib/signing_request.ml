
module Ext = struct

  type _ k =
    | Password : string k
    | Name : string k
    | Extensions : Extension.t k

  module K = struct
    type 'a t = 'a k

    let compare : type a b . a t -> b t -> (a, b) Gmap.Order.t = fun t t' ->
      let open Gmap.Order in
      match t, t' with
      | Password, Password -> Eq | Password, _ -> Lt | _, Password -> Gt
      | Name, Name -> Eq | Name, _ -> Lt | _, Name -> Gt
      | Extensions, Extensions -> Eq
  end

  include Gmap.Make(K)

  let pp_one : type a. a k -> Format.formatter -> a -> unit = fun k ppf v ->
    match k, v with
    | Password, pass -> Fmt.pf ppf "password %s" pass
    | Name, name -> Fmt.pf ppf "name %s" name
    | Extensions, ext -> Fmt.pf ppf "extensions %a" Extension.pp ext

  let pp ppf m = iter (fun (B (k, v)) -> pp_one k ppf v ; Fmt.sp ppf ()) m
end

type request_info = {
  subject : Distinguished_name.t ;
  public_key : Public_key.t ;
  extensions : Ext.t ;
}

type request = {
  info : request_info ;
  signature_algorithm : Algorithm.t ;
  signature : Cstruct.t
}

type t = {
  asn : request ;
  raw : Cstruct.t ;
}

module Asn = struct
  open Asn_grammars
  open Asn.S
  open Registry

  let attributes =
    let f = function[@ocaml.warning "-8"]
      | (oid, [`C1 p]) when oid = PKCS9.challenge_password -> Ext.B (Password, p)
      | (oid, [`C1 n]) when oid = PKCS9.unstructured_name -> Ext.B (Name, n)
      | (oid, [`C2 es]) when oid = PKCS9.extension_request -> Ext.B (Extensions, es)
    and g (Ext.B (k, v)) : Asn.oid * [ `C1 of string | `C2 of Extension.t ] list = match k, v with
      | Ext.Password, v -> (PKCS9.challenge_password, [`C1 v])
      | Ext.Name, v -> (PKCS9.unstructured_name, [`C1 v])
      | Ext.Extensions, v -> (PKCS9.extension_request, [`C2 v])
    in
    map f g @@
    sequence2
      (required ~label:"attr type" oid)
      (required ~label:"attr value"
         (set_of (choice2
                    utf8_string
                    Extension.Asn.extensions_der)))
  let request_info =
    let f = function
      | (0, subject, public_key, extensions) ->
        let extensions =
          List.fold_left (fun map (Ext.B (k, v)) ->
              match Ext.add_unless_bound k v map with
              | None -> parse_error "request extension %a already bound"
                          (Ext.pp_one k) v
              | Some b -> b)
        Ext.empty extensions
        in
        { subject ; public_key ; extensions }
      | _ ->
        parse_error "unknown certificate request info"
    and g { subject ; public_key ; extensions } =
      let extensions = Ext.bindings extensions in
      (0, subject, public_key, extensions)
    in
    map f g @@
    sequence4
      (required ~label:"version" int)
      (required ~label:"subject" Distinguished_name.Asn.name)
      (required ~label:"subjectPKInfo" Public_key.Asn.pk_info_der)
      (required ~label:"attributes" @@ implicit 0 (set_of attributes))

  let request_info_of_cs, request_info_to_cs =
    projections_of Asn.der request_info

  let signing_request =
    let f = fun (info, signature_algorithm, signature) ->
      { info ; signature_algorithm ; signature }
    and g = fun { info ; signature_algorithm ; signature } ->
      (info, signature_algorithm, signature)
    in
    map f g @@
    sequence3
      (required ~label:"certificationRequestInfo" request_info)
      (required ~label:"signatureAlgorithm" Algorithm.identifier)
      (required ~label:"signature" bit_string_cs)

  let signing_request_of_cs, signing_request_to_cs =
    projections_of Asn.der signing_request
end

let info { asn ; _ } = asn.info

let signature_algorithm { asn ; _ } =
  Algorithm.to_signature_algorithm asn.signature_algorithm

let hostnames csr =
  let info = info csr in
  let subj =
    match Distinguished_name.common_name info.subject with
    | None -> Host.Set.empty
    | Some x ->
      match Host.host x with
      | Some (typ, n) -> Host.Set.singleton (typ, n)
      | None -> Host.Set.empty
  in
  match Ext.(find Extensions info.extensions) with
  | None -> subj
  | Some exts -> match Extension.hostnames exts with
    | Some names -> names
    | None -> subj

let validate_signature allowed_hashes { asn ; raw } =
  let raw_data = Validation.raw_cert_hack raw in
  Validation.validate_raw_signature asn.info.subject allowed_hashes raw_data
    asn.signature_algorithm asn.signature asn.info.public_key

let decode_der ?(allowed_hashes = Validation.sha2) cs =
  let open Rresult.R.Infix in
  Asn_grammars.err_to_msg (Asn.signing_request_of_cs cs) >>= fun csr ->
  let csr = { raw = cs ; asn = csr } in
  Rresult.R.error_to_msg ~pp_error:Validation.pp_signature_error
    (validate_signature allowed_hashes csr) >>| fun () ->
  csr

let encode_der { raw ; _ } = raw

let decode_pem cs =
  let open Rresult.R.Infix in
  Pem.parse cs >>= fun data ->
  let crs =
    List.filter (fun (t, _) -> String.equal "CERTIFICATE REQUEST" t) data
  in
  Pem.foldM (fun (_, cs) -> decode_der cs) crs >>=
  Pem.exactly_one ~what:"certificate request"

let encode_pem v =
  Pem.unparse ~tag:"CERTIFICATE REQUEST" (encode_der v)

let digest_of_key = function
  | `RSA _ -> `SHA256
  | `ED25519 _ -> `SHA512
  | `P224 _ -> `SHA224
  | `P256 _ -> `SHA256
  | `P384 _ -> `SHA384
  | `P521 _ -> `SHA512

let default_digest digest key =
  match digest with None -> digest_of_key key | Some x -> x

let create subject ?digest ?(extensions = Ext.empty) (key : Private_key.t) =
  let open Rresult.R.Infix in
  let hash = default_digest digest key in
  let public_key = Private_key.public key in
  let info : request_info = { subject ; public_key ; extensions } in
  let info_cs = Asn.request_info_to_cs info in
  let scheme = Key_type.x509_default_scheme (Private_key.key_type key) in
  Private_key.sign hash ~scheme key (`Message info_cs) >>| fun signature ->
  let signature_algorithm = Algorithm.of_signature_algorithm scheme hash in
  let asn = { info ; signature_algorithm ; signature } in
  let raw = Asn.signing_request_to_cs asn in
  { asn ; raw }

let sign signing_request
    ~valid_from ~valid_until
    ?(allowed_hashes = Validation.sha2)
    ?digest
    ?(serial = Mirage_crypto_pk.(Z_extra.gen_r Z.one Z.(one lsl 64)))
    ?(extensions = Extension.empty)
    ?(subject = signing_request.asn.info.subject)
    key issuer =
  let open Rresult.R.Infix in
  let hash = default_digest digest key in
  validate_signature allowed_hashes signing_request >>= fun () ->
  let signature_algo =
    let scheme = Key_type.x509_default_scheme (Private_key.key_type key) in
    Algorithm.of_signature_algorithm scheme hash
  and info = signing_request.asn.info
  in
  let tbs_cert : Certificate.tBSCertificate = {
    version = `V3 ;
    serial ;
    signature = signature_algo ;
    issuer = issuer ;
    validity = (valid_from, valid_until) ;
    subject ;
    pk_info = info.public_key ;
    issuer_id = None ;
    subject_id = None ;
    extensions
  } in
  let tbs_raw = Certificate.Asn.tbs_certificate_to_cstruct tbs_cert in
  let scheme = Key_type.x509_default_scheme (Private_key.key_type key) in
  Private_key.sign hash ~scheme key (`Message tbs_raw) >>| fun signature_val ->
  let asn = {
    Certificate.tbs_cert ;
    signature_algo ;
    signature_val ;
  } in
  let raw = Certificate.Asn.certificate_to_cstruct asn in
  { Certificate.asn ; raw }
