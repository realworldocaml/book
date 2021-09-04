(* https://tools.ietf.org/html/rfc6960 *)

let version_v1 = 0

(*
 CertID          ::=     SEQUENCE {
      hashAlgorithm       AlgorithmIdentifier,
      issuerNameHash      OCTET STRING, -- Hash of issuer's DN
      issuerKeyHash       OCTET STRING, -- Hash of issuer's public key
      serialNumber        CertificateSerialNumber }
*)
type cert_id = {
  hashAlgorithm: Algorithm.t;
  issuerNameHash: Cstruct.t;
  issuerKeyHash: Cstruct.t;
  serialNumber: Z.t;
}

let create_cert_id ?(hash=`SHA1) issuer serialNumber =
  let hashAlgorithm = Algorithm.of_hash hash in
  let issuerNameHash =
    Certificate.subject issuer
    |> Distinguished_name.encode_der
    |> Mirage_crypto.Hash.digest hash
  in
  let issuerKeyHash =
    Public_key.fingerprint ~hash (Certificate.public_key issuer)
  in
  {hashAlgorithm;issuerNameHash;issuerKeyHash;serialNumber}

let cert_id_serial {serialNumber;_} = serialNumber

let pp_cert_id ppf {hashAlgorithm;issuerNameHash;issuerKeyHash;serialNumber} =
  Fmt.pf ppf "CertID @[<1>{@ algo=%a;@ issuerNameHash=%a;@ issuerKeyHash=%a;@ serialNumber=%a@ }@]"
    Algorithm.pp hashAlgorithm
    Cstruct.hexdump_pp issuerNameHash
    Cstruct.hexdump_pp issuerKeyHash
    Z.pp_print serialNumber

module Asn_common = struct
  open Asn.S

  let cert_id =
    let f (hashAlgorithm, issuerNameHash, issuerKeyHash, serialNumber) =
      {hashAlgorithm;
       issuerNameHash;
       issuerKeyHash;
       serialNumber;}
    in
    let g {hashAlgorithm;issuerNameHash;issuerKeyHash;serialNumber;} =
      (hashAlgorithm, issuerNameHash, issuerKeyHash, serialNumber)
    in
    map f g @@
    sequence4
      (required ~label:"hashAlgorithm" Algorithm.identifier)
      (required ~label:"issuerNameHash" octet_string)
      (required ~label:"issuerKeyHash" octet_string)
      (required ~label:"serialNumber" integer)
end



module Request = struct
  (*
  Request ::= SEQUENCE {
     reqCert                     CertID,
     singleRequestExtensions [0] EXPLICIT Extensions OPTIONAL }
  *)
  type request = {
    reqCert: cert_id;
    singleRequestExtensions: Extension.t option;
  }

  let create_request ?singleRequestExtensions reqCert =
    {reqCert;singleRequestExtensions}

  let pp_request ppf {reqCert;singleRequestExtensions;} =
    Fmt.pf ppf "Request @[<1>{@ reqCert=%a;@ singleRequestExtensions=%a;@ }@]"
      pp_cert_id reqCert
      (Fmt.option ~none:(Fmt.any "None") Extension.pp) singleRequestExtensions

  (*
  TBSRequest      ::=     SEQUENCE {
         version             [0]     EXPLICIT Version DEFAULT v1,
         requestorName       [1]     EXPLICIT GeneralName OPTIONAL,
         requestList                 SEQUENCE OF Request,
         requestExtensions   [2]     EXPLICIT Extensions OPTIONAL }

  *)
  type tbs_request = {
    requestorName: General_name.b option;
    requestList: request list;
    requestExtensions: Extension.t option;
  }

  let create_tbs_request ?requestorName ?requestExtensions requests =
    { requestorName ; requestList=requests ; requestExtensions }

  let pp_tbs_request ppf { requestorName ; requestList ; requestExtensions } =
    let pp_general_name ppf x =
      let open General_name in
      match x with
      | B (k, v) -> General_name.pp_k k ppf v
    in
    Fmt.pf ppf "TBSRequest @[<1>{@ requestorName=%a;@ requestList=[@ %a@ ];@ requestExtensions=%a@ }@]"
      (Fmt.option ~none:(Fmt.any "None") pp_general_name) requestorName
      (Fmt.list ~sep:Fmt.semi pp_request) requestList
      (Fmt.option ~none:(Fmt.any "None") Extension.pp) requestExtensions

  (*
  Signature       ::=     SEQUENCE {
         signatureAlgorithm      AlgorithmIdentifier,
         signature               BIT STRING,
         certs               [0] EXPLICIT SEQUENCE OF Certificate
     OPTIONAL}
  *)
  type signature = {
    signatureAlgorithm: Algorithm.t;
    signature: Cstruct.t;
    certs: Certificate.t list option;
  }

  let pp_signature ppf {signatureAlgorithm;signature;certs;} =
    Fmt.pf ppf "Signature @[<1>{@ signatureAlgorithm=%a;@ signature=%a;@ certs=%a}@]"
      Algorithm.pp signatureAlgorithm
      Cstruct.hexdump_pp signature
      (Fmt.option ~none:(Fmt.any "None") @@
       Fmt.brackets @@
       Fmt.list ~sep:Fmt.semi Certificate.pp) certs

  (*
   OCSPRequest     ::=     SEQUENCE {
         tbsRequest                  TBSRequest,
         optionalSignature   [0]     EXPLICIT Signature OPTIONAL }
  *)
  type req = {
    tbsRequest: tbs_request;
    optionalSignature: signature option;
  }

  type t = {
    raw : Cstruct.t ;
    asn : req ;
  }

  let pp ppf { asn = { tbsRequest ; optionalSignature } ; _ } =
    Fmt.pf ppf "OCSPRequest @[<1>{@ tbsRequest=%a;@ optionalSignature=%a@ }@]"
      pp_tbs_request tbsRequest
      (Fmt.option ~none:(Fmt.any "None") pp_signature) optionalSignature

  let cert_ids { asn = { tbsRequest = { requestList ; _ } ; _ } ; _ } =
    let cert_ids = List.map (fun {reqCert;_} -> reqCert) requestList in
    cert_ids

  let requestor_name { asn = { tbsRequest = { requestorName ; _ } ; _ } ; _ } =
    requestorName

  module Asn_ = Asn

  module Asn = struct
    open Asn_grammars
    open Asn.S

    let request =
      let f (reqCert, singleRequestExtensions) =
        {reqCert; singleRequestExtensions}
      in
      let g {reqCert; singleRequestExtensions} =
        (reqCert, singleRequestExtensions)
      in
      map f g @@
      sequence2
        (required ~label:"reqCert" Asn_common.cert_id)
        (optional ~label:"singleRequestExtensions" @@ explicit 0
           Extension.Asn.extensions_der)

    let tbs_request =
      let f (version, requestorName, requestList, requestExtensions) =
        match version with
        | Some v when v <> version_v1 ->
          Asn.S.parse_error "unsupported version %d" v
        | _ ->
          { requestorName ; requestList ; requestExtensions }
      in
      let g { requestorName ; requestList ; requestExtensions } =
        (None, requestorName, requestList, requestExtensions)
      in
      map f g @@
      sequence4
        (optional ~label:"version" @@ explicit 0 int)
        (optional ~label:"requestorName" @@
         explicit 1 General_name.Asn.general_name)
        (required ~label:"requestList" @@ sequence_of request)
        (optional ~label:"requestExtensions" @@ Extension.Asn.extensions_der)

    let tbs_request_of_cs,tbs_request_to_cs =
      projections_of Asn.der tbs_request

    let signature =
      let f (signatureAlgorithm,signature,certs) =
        let certs = match certs with
          | None -> None
          | Some certs ->
            let encode cert =
              let raw = Certificate.Asn.certificate_to_cstruct cert in
              Certificate.{raw; asn=cert}
            in
            Some (List.map encode certs)
        in
        {signatureAlgorithm;signature;certs}
      in
      let g {signatureAlgorithm;signature;certs} =
        let certs = match certs with
          | None -> None
          | Some certs ->
            Some (List.map (fun Certificate.{asn;_} -> asn) certs)
        in
        (signatureAlgorithm,signature,certs)
      in
      map f g @@
      sequence3
        (required ~label:"signatureAlgorithm" Algorithm.identifier)
        (required ~label:"signature" bit_string_cs)
        (optional ~label:"certs" @@ explicit 0 @@
         sequence_of Certificate.Asn.certificate)

    let ocsp_request =
      let f (tbsRequest,optionalSignature) =
        {tbsRequest;optionalSignature;}
      in
      let g {tbsRequest;optionalSignature;} =
        (tbsRequest,optionalSignature)
      in
      map f g @@
      sequence2
        (required ~label:"tbsRequest" tbs_request)
        (optional ~label:"optionalSignature" signature)

    let (ocsp_request_of_cstruct, ocsp_request_to_cstruct) =
      projections_of Asn.der ocsp_request

  end

  let decode_der raw =
    let open Rresult.R.Infix in
    Asn.ocsp_request_of_cstruct raw >>| fun asn ->
    { asn ; raw }

  let encode_der { raw ; _ } = raw

  let create ?certs ?digest ?requestor_name:requestorName ?key cert_ids =
    let requestList = List.map create_request cert_ids in
    let tbsRequest = {
      requestorName;
      requestList;
      requestExtensions=None;
    }
    in
    let open Rresult.R.Infix in
    (match key with
     | None -> Ok None
     | Some key ->
       let digest = Signing_request.default_digest digest key in
       let scheme = Key_type.x509_default_scheme (Private_key.key_type key) in
       let signatureAlgorithm = Algorithm.of_signature_algorithm scheme digest in
       let tbs_der = Asn.tbs_request_to_cs tbsRequest in
       Private_key.sign digest ~scheme key (`Message tbs_der) >>| fun signature ->
       Some { signature ; signatureAlgorithm ; certs; }) >>| fun optionalSignature ->
    let asn = { tbsRequest ; optionalSignature } in
    let raw = Asn.ocsp_request_to_cstruct asn in
    { raw ; asn }

  let validate { asn ; raw } ?(allowed_hashes = Validation.sha2) pub =
    match asn.optionalSignature with
    | None -> Error `No_signature
    | Some sign ->
      let tbs_raw = Validation.raw_cert_hack raw in
      let dn =
        let cn = "OCSP" in
        [ Distinguished_name.(Relative_distinguished_name.singleton (CN cn)) ]
      in
      Validation.validate_raw_signature dn allowed_hashes tbs_raw
        sign.signatureAlgorithm sign.signature pub
end

module Response = struct

   (* OCSPResponseStatus ::= ENUMERATED {
    *     successful            (0),  -- Response has valid confirmations
    *     malformedRequest      (1),  -- Illegal confirmation request
    *     internalError         (2),  -- Internal error in issuer
    *     tryLater              (3),  -- Try again later
    *                                 -- (4) is not used
    *     sigRequired           (5),  -- Must sign the request
    *     unauthorized          (6)   -- Request unauthorized
    * } *)
  type status = [
    | `Successful
    | `MalformedRequest
    | `InternalError
    | `TryLater
    | `SigRequired
    | `Unauthorized
  ]

  let status_to_int = function
    | `Successful -> 0
    | `MalformedRequest -> 1
    | `InternalError -> 2
    | `TryLater -> 3
    | `SigRequired -> 5
    | `Unauthorized -> 6

  let status_of_int = function
    |  0 -> `Successful
    |  1 -> `MalformedRequest
    |  2 -> `InternalError
    |  3 -> `TryLater
    |  5 -> `SigRequired
    |  6 -> `Unauthorized
    | x -> Asn.S.parse_error "Unknown status %d" x


  let pp_status ppf = function
    | `Successful -> Fmt.string ppf "Successful"
    | `MalformedRequest -> Fmt.string ppf "MalformedRequest"
    | `InternalError -> Fmt.string ppf "InternalError"
    | `TryLater -> Fmt.string ppf "TryLater"
    | `SigRequired -> Fmt.string ppf "SigRequired"
    | `Unauthorized -> Fmt.string ppf "Unauthorized"

  (* RevokedInfo ::= SEQUENCE {
   *   revocationTime              GeneralizedTime,
   *   revocationReason    [0]     EXPLICIT CRLReason OPTIONAL } *)
  type revoked_info = Ptime.t * Extension.reason option

  let pp_revoked_info ppf (revocationTime,revocationReason) =
    Fmt.pf ppf "RevokedInfo @[<1>{@ revocationTime=%a;@ revocationReason=%a;@ }@]"
      Ptime.pp revocationTime
      (Fmt.option ~none:(Fmt.any "None") @@ Extension.pp_reason)
      revocationReason

   (* CertStatus ::= CHOICE {
    *     good        [0]     IMPLICIT NULL,
    *     revoked     [1]     IMPLICIT RevokedInfo,
    *     unknown     [2]     IMPLICIT UnknownInfo } *)

  type cert_status = [
    | `Good
    | `Revoked of revoked_info
    | `Unknown
  ]

  let pp_cert_status ppf = function
    | `Good -> Fmt.pf ppf "Good"
    | `Revoked info -> Fmt.pf ppf "Revoked of %a" pp_revoked_info info
    | `Unknown -> Fmt.pf ppf "Unknown"

   (* SingleResponse ::= SEQUENCE {
    *  certID                       CertID,
    *  certStatus                   CertStatus,
    *  thisUpdate                   GeneralizedTime,
    *  nextUpdate         [0]       EXPLICIT GeneralizedTime OPTIONAL,
    *  singleExtensions   [1]       EXPLICIT Extensions OPTIONAL } *)

  type single_response = {
    certID: cert_id;
    certStatus: cert_status;
    thisUpdate: Ptime.t;
    nextUpdate: Ptime.t option;
    singleExtensions: Extension.t option;
  }

  let create_single_response ?next_update:nextUpdate
      ?single_extensions:singleExtensions
      certID certStatus thisUpdate =
    {certID;certStatus;thisUpdate;nextUpdate;singleExtensions;}

  let pp_single_response ppf {certID;certStatus;thisUpdate;nextUpdate;singleExtensions;} =
    Fmt.pf ppf "SingleResponse @[<1>{@ certID=%a;@ certStatus=%a;@ thisUpdate=%a;@ nextUpdate=%a;@ singleExtensions=%a;@ }@]"
      pp_cert_id certID
      pp_cert_status certStatus
      Ptime.pp thisUpdate
      (Fmt.option ~none:(Fmt.any "None") @@ Ptime.pp) nextUpdate
      (Fmt.option ~none:(Fmt.any "None") @@ Extension.pp) singleExtensions

  let single_response_cert_id {certID;_} = certID

  let single_response_status {certStatus;_} = certStatus

 (* ResponderID ::= CHOICE {
  *    byName               [1] Name,
  *    byKey                [2] KeyHash }
  *   KeyHash ::= OCTET STRING -- SHA-1 hash of responder's public key
   (excluding the tag and length fields)
  *)
  type responder_id = [
    | `ByName of Distinguished_name.t
    | `ByKey of Cstruct.t
  ]

  let create_responder_id pubkey =
    let pubkey_fp = Public_key.fingerprint ~hash:`SHA1 pubkey in
    `ByKey pubkey_fp

  let pp_responder_id ppf = function
    | `ByName dn -> Fmt.pf ppf "ByName %a" Distinguished_name.pp dn
    | `ByKey hash -> Fmt.pf ppf "ByKey %a" Cstruct.hexdump_pp hash

  (* ResponseData ::= SEQUENCE {
   *  version              [0] EXPLICIT Version DEFAULT v1,
   *  responderID              ResponderID,
   *  producedAt               GeneralizedTime,
   *  responses                SEQUENCE OF SingleResponse,
   *  responseExtensions   [1] EXPLICIT Extensions OPTIONAL } *)
  type response_data = {
    responderID: responder_id;
    producedAt: Ptime.t;
    responses: single_response list;
    responseExtensions: Extension.t option;
  }

  let pp_response_data ppf { responderID ; producedAt ; responses ; responseExtensions } =
    Fmt.pf ppf "ResponseData @[<1>{@ responderID=%a;@ producedAt=%a;@ responses=%a;@ responseExtensions=%a@ }@]"
      pp_responder_id responderID
      Ptime.pp producedAt
      (Fmt.list ~sep:Fmt.semi @@ pp_single_response) responses
      (Fmt.option ~none:(Fmt.any "None") @@ Extension.pp) responseExtensions

   (* BasicOCSPResponse       ::= SEQUENCE {
    *    tbsResponseData      ResponseData,
    *    signatureAlgorithm   AlgorithmIdentifier,
    *    signature            BIT STRING,
    *    certs            [0] EXPLICIT SEQUENCE OF Certificate OPTIONAL } *)
  type basic_ocsp_response = {
    tbsResponseData: response_data;
    signatureAlgorithm: Algorithm.t;
    signature: Cstruct.t;
    certs: Certificate.t list option;
  }

  let pp_basic_ocsp_response ppf {tbsResponseData;signatureAlgorithm;signature;certs;} =
    Fmt.pf ppf "BasicOCSPResponse @[<1>{@ tbsResponseData=%a;@ signatureAlgorithm=%a;@ signature=%a;@ certs=%a@ }@]"
      pp_response_data tbsResponseData
      Algorithm.pp signatureAlgorithm
      Cstruct.hexdump_pp signature
      (Fmt.option ~none:(Fmt.any "None") @@
       Fmt.list ~sep:Fmt.semi @@ Certificate.pp) certs

  (* ResponseBytes ::=       SEQUENCE {
   *     responseType   OBJECT IDENTIFIER,
   *     response       OCTET STRING } *)

  (* OCSPResponse ::= SEQUENCE {
   *     responseStatus         OCSPResponseStatus,
   *     responseBytes          [0] EXPLICIT ResponseBytes OPTIONAL } *)

  type t = {
    responseStatus: status;
    responseBytes: (Asn.oid * basic_ocsp_response * Cstruct.t) option;
  }

  let pp ppf {responseStatus;responseBytes;} =
    Fmt.pf ppf "OCSPResponse @[<1>{@ responseStatus=%a;@ responseBytes=%a@ }@]"
      pp_status responseStatus
      (Fmt.option ~none:(Fmt.any "None") @@
       Fmt.pair ~sep:Fmt.comma Asn.OID.pp pp_basic_ocsp_response)
      (match responseBytes with None -> None | Some (a, b, _) -> Some (a, b))

  let status {responseStatus;_} = responseStatus

  let responder_id = function
    | {responseBytes=Some (_, {tbsResponseData={responderID;_};_}, _);_} ->
      Ok responderID
    | _ -> Error (`Msg "this response has no responseBytes")

  let responses = function
    | {responseBytes=Some (_, {tbsResponseData={responses;_};_}, _);_} ->
      Ok responses
    | _ -> Error (`Msg "this response has no responseBytes")

  module Asn = struct
    open Asn_grammars
    open Asn.S
    open Registry

    let status : status Asn.t =
      enumerated status_of_int status_to_int

    let revoked_info =
      sequence2
        (required ~label:"revocationTime" generalized_time_no_frac_s)
        (optional ~label:"revocationReason" @@ explicit 0 @@
         Extension.Asn.reason_enumerated)

    let cert_status : cert_status Asn.t =
      let f = function
        | `C1 () -> `Good
        | `C2 ri -> `Revoked ri
        | `C3 () -> `Unknown
      in
      let g = function
        | `Good -> `C1 ()
        | `Revoked ri -> `C2 ri
        | `Unknown -> `C3 ()
      in
      map f g @@
      choice3
        (implicit 0 @@ null)
        (implicit 1 @@ revoked_info)
        (implicit 2 @@ null)

    let single_response =
      let f (certID,certStatus,thisUpdate,nextUpdate,singleExtensions) =
        {certID;certStatus;thisUpdate;nextUpdate;singleExtensions;}
      in
      let g {certID;certStatus;thisUpdate;nextUpdate;singleExtensions;} =
        (certID,certStatus,thisUpdate,nextUpdate,singleExtensions)
      in
      map f g @@
      sequence5
        (required ~label:"certID" @@ Asn_common.cert_id)
        (required ~label:"certStatus" @@ cert_status)
        (required ~label:"thisUpdate" @@ generalized_time_no_frac_s)
        (optional ~label:"nextUpdate" @@ explicit 0 @@
         generalized_time_no_frac_s)
        (optional ~label:"singleExtensions" @@ explicit 1 @@
         Extension.Asn.extensions_der)

    let responder_id : responder_id Asn.t =
      let f = function
        | `C1 dn -> `ByName dn
        | `C2 hash -> `ByKey hash
      in
      let g = function
        | `ByName dn -> `C1 dn
        | `ByKey hash -> `C2 hash
      in
      map f g @@
      choice2 (explicit 1 Distinguished_name.Asn.name) (explicit 2 octet_string)

    let response_data =
      let f (version, responderID, producedAt, responses, responseExtensions) =
        match version with
        | Some v when v <> version_v1 ->
          Asn.S.parse_error "unsupported version %d" v
        | _ -> { responderID ; producedAt ; responses ; responseExtensions }
      in
      let g { responderID ; producedAt ; responses ; responseExtensions } =
        (None, responderID, producedAt, responses, responseExtensions)
      in
      map f g @@
      sequence5
        (optional ~label:"version" @@ explicit 0 @@ int)
        (required ~label:"responderID" responder_id)
        (required ~label:"producedAt" generalized_time_no_frac_s)
        (required ~label:"responses" @@ sequence_of single_response)
        (optional ~label:"responseExtensions" @@ explicit 1 @@
         Extension.Asn.extensions_der)

    let response_data_of_cs, response_data_to_cs =
      projections_of Asn.der response_data

    let basic_ocsp_response =
      let f (tbsResponseData,signatureAlgorithm,signature,certs) =
        let certs = match certs with
          | None -> None
          | Some certs ->
            let encode cert =
              let raw = Certificate.Asn.certificate_to_cstruct cert in
              Certificate.{raw; asn=cert}
            in
            Some (List.map encode certs)
        in
        {tbsResponseData;signatureAlgorithm;signature;certs}
      in
      let g {tbsResponseData;signatureAlgorithm;signature;certs} =
        let certs = match certs with
          | None -> None
          | Some certs ->
            Some (List.map (fun Certificate.{asn;_} -> asn) certs)
        in
        (tbsResponseData,signatureAlgorithm,signature,certs)
      in
      map f g @@
      sequence4
        (required ~label:"tbsResponseData" response_data)
        (required ~label:"signatureAlgorithm" Algorithm.identifier)
        (required ~label:"signature" bit_string_cs)
        (optional ~label:"certs" @@ explicit 0 @@
         sequence_of Certificate.Asn.certificate)

    let basic_ocsp_response_of_cs,basic_ocsp_response_to_cs =
      projections_of Asn.der basic_ocsp_response

    let ocsp_basic_oid = Cert_extn.Private_internet_extensions.ad_ocsp_basic

    let ocsp_response =
      let f = function
        | `Successful, None ->
          parse_error "Successful status requires responseBytes"
        | `Successful, Some (oid, response) ->
          if Asn.OID.equal ocsp_basic_oid oid then
            match basic_ocsp_response_of_cs response with
            | Error e -> error e
            | Ok basic_response ->
              {responseStatus=`Successful;
               responseBytes=Some (oid, basic_response, response)}
          else
            parse_error "expected OID ad_ocsp_basic"
        | (`InternalError
          | `MalformedRequest
          | `SigRequired
          | `TryLater
          |`Unauthorized) as s, None ->
          {responseStatus=s;responseBytes=None}
        | _, Some _ -> parse_error "Only Successful status supports non empty responseBytes"
      in
      let g {responseStatus;responseBytes} =
        let responseBytes = match responseBytes with
          | Some (oid, _basic_response, response) -> Some (oid, response)
          | None -> None
        in
        (responseStatus,responseBytes)
      in
      map f g @@
      sequence2
        (required ~label:"responseStatus" status)
        (optional ~label:"responseBytes" @@ explicit 0 @@
         sequence2
           (required ~label:"responseType" oid)
           (required ~label:"response" octet_string))

    let ocsp_response_of_cs, ocsp_response_to_cs =
      projections_of Asn.der ocsp_response

  end

  let decode_der = Asn.ocsp_response_of_cs
  let encode_der = Asn.ocsp_response_to_cs

  let create_basic_ocsp_response ?digest ?certs
      ?response_extensions:responseExtensions key responderID producedAt
      responses =
    let digest = Signing_request.default_digest digest key in
    let scheme = Key_type.x509_default_scheme (Private_key.key_type key) in
    let signatureAlgorithm = Algorithm.of_signature_algorithm scheme digest in
    let tbsResponseData = {
      responderID;
      producedAt;
      responses;
      responseExtensions;
    } in
    let resp_der = Asn.response_data_to_cs tbsResponseData in
    let open Rresult.R.Infix in
    Private_key.sign digest ~scheme key (`Message resp_der) >>| fun signature ->
    {tbsResponseData;signatureAlgorithm;signature;certs;}

  let create_success ?digest ?certs ?response_extensions
      private_key responderID producedAt responses =
    let open Rresult.R.Infix in
    create_basic_ocsp_response
      ?digest ?certs ?response_extensions private_key
      responderID producedAt responses >>| fun response ->
    let raw_resp = Asn.basic_ocsp_response_to_cs response in
    let responseBytes = Some (Asn.ocsp_basic_oid, response, raw_resp) in
    {responseStatus=`Successful; responseBytes}

  let create status =
    let status = match status with
      | `MalformedRequest -> `MalformedRequest
      | `InternalError -> `InternalError
      | `TryLater -> `TryLater
      | `SigRequired -> `SigRequired
      | `Unauthorized -> `Unauthorized
    in
    {responseStatus=status;responseBytes=None}

  let validate t ?(allowed_hashes = Validation.sha2) ?now pub =
    match t.responseBytes with
    | None -> Error `No_signature
    | Some (_oid, response, raw_resp) ->
      let resp_der = Validation.raw_cert_hack raw_resp in
      let dn =
        let cn = "OCSP" in
        [ Distinguished_name.(Relative_distinguished_name.singleton (CN cn)) ]
      in
      let open Rresult.R.Infix in
      Validation.validate_raw_signature dn allowed_hashes resp_der
        response.signatureAlgorithm response.signature pub >>= fun () ->
      match now with
      | None -> Ok ()
      | Some now ->
        if
          List.for_all (fun single_resp ->
              Ptime.is_later ~than:single_resp.thisUpdate now &&
              match single_resp.nextUpdate with
              | None -> true
              | Some until -> Ptime.is_earlier ~than:until now)
            response.tbsResponseData.responses
        then
          Ok ()
        else
          Error `Time_invalid
end
