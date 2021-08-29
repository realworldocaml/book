open! Core
open! Async

let file_contents file =
  Deferred.Or_error.try_with ~name:(sprintf "read %s" file) (fun () ->
    Reader.file_contents file)
;;

let load_all_in_directory ~directory ~f =
  let open Deferred.Or_error.Let_syntax in
  let options = Async_find.Options.ignore_errors in
  let%bind files = Async_find.find_all ~options directory |> Deferred.ok in
  Deferred.Or_error.List.map files ~f:(fun (file, (_ : Unix.Stats.t)) ->
    let%bind contents = file_contents file in
    f ~contents)
;;

module Or_error = struct
  include Or_error

  let of_result ~to_string = Result.map_error ~f:(Fn.compose Error.of_string to_string)
  let of_result_msg x = of_result x ~to_string:(fun (`Msg msg) -> msg)

  let lift_result_msg_of_cstruct f ~contents =
    f (Cstruct.of_string contents) |> of_result_msg
  ;;

  let lift_asn_error_of_cstruct f ~contents =
    f (Cstruct.of_string contents) |> of_result ~to_string:(fun (`Parse msg) -> msg)
  ;;
end

module CRL = struct
  include X509.CRL

  let decode_der = Or_error.lift_result_msg_of_cstruct decode_der

  let revoke ?digest ~issuer ~this_update ?next_update ?extensions revoked_certs key =
    revoke ?digest ~issuer ~this_update ?next_update ?extensions revoked_certs key
    |> Or_error.of_result_msg
  ;;

  let revoke_certificate revoked ~this_update ?next_update crl key =
    revoke_certificate revoked ~this_update ?next_update crl key |> Or_error.of_result_msg
  ;;

  let revoke_certificates revoked ~this_update ?next_update crl key =
    revoke_certificates revoked ~this_update ?next_update crl key
    |> Or_error.of_result_msg
  ;;

  let of_pem_dir ~directory =
    load_all_in_directory ~directory ~f:(fun ~contents ->
      decode_der ~contents |> Deferred.return)
  ;;
end

module Certificate = struct
  include X509.Certificate
  open Deferred.Or_error.Let_syntax

  let decode_pem_multiple = Or_error.lift_result_msg_of_cstruct decode_pem_multiple
  let decode_pem = Or_error.lift_result_msg_of_cstruct decode_pem
  let decode_der = Or_error.lift_result_msg_of_cstruct decode_der

  let of_pem_file ca_file =
    let%bind contents = file_contents ca_file in
    decode_pem_multiple ~contents |> Deferred.return
  ;;

  let of_pem_directory ~directory =
    load_all_in_directory ~directory ~f:(fun ~contents ->
      decode_pem_multiple ~contents |> Deferred.return)
    >>| List.concat
  ;;
end

module Authenticator = struct
  include X509.Authenticator

  module Param = struct
    module Chain_of_trust = struct
      type t =
        { trust_anchors : [ `File of Filename.t | `Directory of Filename.t ]
        ; allowed_hashes : Mirage_crypto.Hash.hash list option
        ; crls : Filename.t option
        }

      let to_certs = function
        | `File file -> Certificate.of_pem_file file
        | `Directory directory -> Certificate.of_pem_directory ~directory
      ;;
    end

    type t =
      | Chain_of_trust of Chain_of_trust.t
      | Cert_fingerprints of
          Mirage_crypto.Hash.hash * ([ `host ] Domain_name.t * string) list

    let ca_file ?allowed_hashes ?crls filename () =
      let trust_anchors = `File filename in
      Chain_of_trust { trust_anchors; allowed_hashes; crls }
    ;;

    let ca_dir ?allowed_hashes ?crls directory_name () =
      let trust_anchors = `Directory directory_name in
      Chain_of_trust { trust_anchors; allowed_hashes; crls }
    ;;

    let cert_fingerprints hash fingerprints = Cert_fingerprints (hash, fingerprints)

    let cleanup_fingerprint fingerprint =
      let known_delimiters = [ ':'; ' ' ] in
      String.filter fingerprint ~f:(fun c ->
        not (List.exists known_delimiters ~f:(Char.equal c)))
      |> Cstruct.of_hex
    ;;

    let of_cas ~time ({ trust_anchors; allowed_hashes; crls } : Chain_of_trust.t) =
      let open Deferred.Or_error.Let_syntax in
      let%bind cas = Chain_of_trust.to_certs trust_anchors in
      let%map crls =
        match crls with
        | Some directory ->
          let%map crls = CRL.of_pem_dir ~directory in
          Some crls
        | None -> return None
      in
      X509.Authenticator.chain_of_trust ?allowed_hashes ?crls ~time cas
    ;;

    let cert_fingerprint ~time hash fingerprints =
      let fingerprints =
        List.map fingerprints ~f:(Tuple.T2.map_snd ~f:cleanup_fingerprint)
      in
      X509.Authenticator.server_cert_fingerprint ~time ~hash ~fingerprints
    ;;

    let time = Fn.compose Ptime.of_float_s Unix.gettimeofday

    let to_authenticator ~time param =
      match param with
      | Chain_of_trust chain_of_trust -> of_cas ~time chain_of_trust
      | Cert_fingerprints (hash, fingerprints) ->
        cert_fingerprint ~time hash fingerprints |> Deferred.Or_error.return
    ;;
  end
end

module Distinguished_name = struct
  include X509.Distinguished_name

  let decode_der = Or_error.lift_result_msg_of_cstruct decode_der
end

module OCSP = struct
  include X509.OCSP

  module Request = struct
    include Request

    let create ?certs ?digest ?requestor_name ?key cert_ids =
      create ?certs ?digest ?requestor_name ?key cert_ids |> Or_error.of_result_msg
    ;;

    let decode_der = Or_error.lift_asn_error_of_cstruct decode_der
  end

  module Response = struct
    include Response

    let create_success
          ?digest
          ?certs
          ?response_extensions
          private_key
          responderID
          producedAt
          responses
      =
      create_success
        ?digest
        ?certs
        ?response_extensions
        private_key
        responderID
        producedAt
        responses
      |> Or_error.of_result_msg
    ;;

    let responses t = responses t |> Or_error.of_result_msg
    let decode_der = Or_error.lift_asn_error_of_cstruct decode_der
  end
end

module PKCS12 = struct
  include X509.PKCS12

  let decode_der = Or_error.lift_result_msg_of_cstruct decode_der
  let verify password t = verify password t |> Or_error.of_result_msg
end

module Private_key = struct
  include X509.Private_key

  let sign hash ?scheme key data =
    sign hash ?scheme key data
    |> Or_error.of_result_msg
    |> Or_error.map ~f:Cstruct.to_string
  ;;

  let decode_der = Or_error.lift_result_msg_of_cstruct decode_der
  let decode_pem = Or_error.lift_result_msg_of_cstruct decode_pem

  let of_pem_file file =
    let%map contents = Reader.file_contents file in
    decode_pem ~contents
  ;;
end

module Public_key = struct
  include X509.Public_key

  let verify hash ?scheme ~signature key data =
    let signature = Cstruct.of_string signature in
    let data =
      match data with
      | `Digest data -> `Digest (Cstruct.of_string data)
      | `Message data -> `Message (Cstruct.of_string data)
    in
    verify hash ?scheme ~signature key data |> Or_error.of_result_msg
  ;;

  let decode_der = Or_error.lift_result_msg_of_cstruct decode_der
  let decode_pem = Or_error.lift_result_msg_of_cstruct decode_pem
end

module Signing_request = struct
  include X509.Signing_request

  let decode_der ?allowed_hashes der =
    Cstruct.of_string der |> decode_der ?allowed_hashes |> Or_error.of_result_msg
  ;;

  let decode_pem pem = Cstruct.of_string pem |> decode_pem |> Or_error.of_result_msg

  let create subject ?digest ?extensions key =
    create subject ?digest ?extensions key |> Or_error.of_result_msg
  ;;

  let sign
        ?allowed_hashes
        ?digest
        ?serial
        ?extensions
        t
        key
        issuer
        ~valid_from
        ~valid_until
    =
    sign ?allowed_hashes ?digest ?serial ?extensions t key issuer ~valid_from ~valid_until
    |> Or_error.of_result ~to_string:(Fmt.to_to_string X509.Validation.pp_signature_error)
  ;;
end

module Extension = X509.Extension
module General_name = X509.General_name
module Host = X509.Host
module Key_type = X509.Key_type
module Validation = X509.Validation
