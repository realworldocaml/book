open Packet
open Core
open Cstruct

open Sexplib.Conv

type error =
  | TrailingBytes  of string
  | WrongLength    of string
  | Unknown        of string
  | Underflow
  | Overflow       of int
  | UnknownVersion of (int * int)
  | UnknownContent of int
  [@@deriving sexp]

include Control.Or_error_make (struct type err = error end)
type nonrec 'a result = ('a, error) result

exception Reader_error of error

let raise_unknown msg        = raise (Reader_error (Unknown msg))
and raise_wrong_length msg   = raise (Reader_error (WrongLength msg))
and raise_trailing_bytes msg = raise (Reader_error (TrailingBytes msg))

let catch f x =
  try return (f x) with
  | Reader_error err   -> fail err
  | Invalid_argument _ -> fail Underflow

let parse_version_int buf =
  let major = get_uint8 buf 0 in
  let minor = get_uint8 buf 1 in
  (major, minor)

let parse_version_exn buf =
  let version = parse_version_int buf in
  match tls_version_of_pair version with
  | Some x -> x
  | None   ->
    let major, minor = version in
    raise (Reader_error (UnknownVersion (major, minor)))

let parse_any_version_opt buf =
  let version = parse_version_int buf in
  tls_any_version_of_pair version, shift buf 2

let parse_any_version_exn buf =
  match parse_any_version_opt buf with
  | Some x, _ -> x
  | None, _ ->
    let major, minor = (get_uint8 buf 0, get_uint8 buf 1) in
    raise (Reader_error (UnknownVersion (major, minor)))

let parse_version = catch parse_version_exn

let parse_any_version = catch parse_any_version_exn

let parse_record buf =
  if len buf < 5 then
    return (`Fragment buf)
  else
    let typ = get_uint8 buf 0
    and version = parse_version_int (shift buf 1)
    in
    match BE.get_uint16 buf 3 with
    | x when x > (1 lsl 14 + 2048) ->
      (* 2 ^ 14 + 2048 for TLSCiphertext
         2 ^ 14 + 1024 for TLSCompressed
         2 ^ 14 for TLSPlaintext *)
      fail (Overflow x)
    | x when 5 + x > len buf -> return (`Fragment buf)
    | x ->
      match
        tls_any_version_of_pair version,
        int_to_content_type typ
      with
      | None, _ -> fail (UnknownVersion version)
      | _, None -> fail (UnknownContent typ)
      | Some version, Some content_type ->
        let payload, rest = split ~start:5 buf x in
        return (`Record (({ content_type ; version }, payload), rest))

let validate_alert (lvl, typ) =
  let open Packet in
  match lvl, typ with
  (* from RFC, find out which ones must be always FATAL
     and report if this does not meet the expectations *)
  | WARNING, UNEXPECTED_MESSAGE -> raise_unknown "unexpected_message must always be fatal"
  | WARNING, BAD_RECORD_MAC -> raise_unknown "bad_record_mac must always be fatal"
  | WARNING, DECRYPTION_FAILED -> raise_unknown "decryption_failed must always be fatal"
  | WARNING, RECORD_OVERFLOW -> raise_unknown "record_overflow must always be fatal"
  | WARNING, DECOMPRESSION_FAILURE -> raise_unknown "decompression_failure must always be fatal"
  | WARNING, HANDSHAKE_FAILURE -> raise_unknown "handshake_failure must always be fatal"
  | WARNING, BAD_CERTIFICATE -> raise_unknown "bad_certificate must always be fatal"
  | WARNING, UNSUPPORTED_CERTIFICATE -> raise_unknown "unsupported_certificate must always be fatal"
  | WARNING, CERTIFICATE_REVOKED -> raise_unknown "certificate_revoked must always be fatal"
  | WARNING, CERTIFICATE_UNKNOWN -> raise_unknown "certificate_unknown must always be fatal"
  | WARNING, ILLEGAL_PARAMETER -> raise_unknown "illegal_parameter must always be fatal"
  | WARNING, UNKNOWN_CA -> raise_unknown "unknown_ca must always be fatal"
  | WARNING, ACCESS_DENIED -> raise_unknown "access_denied must always be fatal"
  | WARNING, DECODE_ERROR -> raise_unknown "decode_error must always be fatal"
  | WARNING, DECRYPT_ERROR -> raise_unknown "decrypt_error must always be fatal"
  | WARNING, PROTOCOL_VERSION -> raise_unknown "protocol_version must always be fatal"
  | WARNING, INSUFFICIENT_SECURITY -> raise_unknown "insufficient_security must always be fatal"
  | WARNING, INTERNAL_ERROR -> raise_unknown "internal_error must always be fatal"
  | WARNING, INAPPROPRIATE_FALLBACK -> raise_unknown "inappropriate_fallback must always be fatal"
  | WARNING, MISSING_EXTENSION -> raise_unknown "missing_extension must always be fatal"
  | WARNING, UNSUPPORTED_EXTENSION -> raise_unknown "unsupported_extension must always be fatal"
  | WARNING, UNRECOGNIZED_NAME -> raise_unknown "unrecognized_name must always be fatal"
  | WARNING, BAD_CERTIFICATE_STATUS_RESPONSE -> raise_unknown "bad_certificate_status_response must always be fatal"
  | WARNING, UNKNOWN_PSK_IDENTITY -> raise_unknown "unknown_psk_identity must always be fatal"
  | WARNING, CERTIFICATE_REQUIRED -> raise_unknown "certificate_required must always be fatal"
  | WARNING, NO_APPLICATION_PROTOCOL -> raise_unknown "no_application_protocol must always be fatal"

  (* those are always warnings *)
  | FATAL, USER_CANCELED -> raise_unknown "user_canceled must always be a warning"
  | FATAL, NO_RENEGOTIATION -> raise_unknown "no_renegotiation must always be a warning"

  | lvl, typ -> (lvl, typ)

let parse_alert = catch @@ fun buf ->
  if len buf <> 2 then
    raise_trailing_bytes "after alert"
  else
    let level = get_uint8 buf 0 in
    let typ = get_uint8 buf 1 in
    match int_to_alert_level level, int_to_alert_type typ with
      | (Some lvl, Some msg) -> validate_alert (lvl, msg)
      | (Some _  , None)     -> raise_unknown @@ "alert type " ^ string_of_int typ
      | _                    -> raise_unknown @@ "alert level " ^ string_of_int level

let parse_change_cipher_spec buf =
  match len buf, get_uint8 buf 0 with
  | 1, 1 -> return ()
  | _    -> fail (Unknown "bad change cipher spec message")

let rec parse_count_list parsef buf acc = function
  | 0 -> (List.rev acc, buf)
  | n ->
     match parsef buf with
     | Some elem, buf' -> parse_count_list parsef buf' (elem :: acc) (pred n)
     | None     , buf' -> parse_count_list parsef buf'          acc  (pred n)

let rec parse_list parsef buf acc =
  match len buf with
  | 0 -> List.rev acc
  | _ ->
     match parsef buf with
     | Some elem, buf' -> parse_list parsef buf' (elem :: acc)
     | None     , buf' -> parse_list parsef buf'          acc

let parse_compression_method buf =
  let cm = get_uint8 buf 0 in
  (int_to_compression_method cm, shift buf 1)

let parse_compression_methods buf =
  let count = get_uint8 buf 0 in
  parse_count_list parse_compression_method (shift buf 1) [] count

let parse_any_ciphersuite buf =
  let typ = BE.get_uint16 buf 0 in
  (int_to_any_ciphersuite typ, shift buf 2)

let parse_any_ciphersuites buf =
  let count = BE.get_uint16 buf 0 in
  if count mod 2 <> 0 then
    raise_wrong_length "ciphersuite list"
  else
    parse_count_list parse_any_ciphersuite (shift buf 2) [] (count / 2)

let parse_ciphersuite buf =
  match parse_any_ciphersuite buf with
  | None   , buf' -> (None, buf')
  | Some cs, buf' -> match Ciphersuite.any_ciphersuite_to_ciphersuite cs with
                       | None     -> (None, buf')
                       | Some cs' -> (Some cs', buf')

let parse_hostnames buf =
  match len buf with
  | 0 -> []
  | n ->
     let parsef buf =
       let typ = get_uint8 buf 0 in
       let entrylen = BE.get_uint16 buf 1 in
       let rt = shift buf (3 + entrylen) in
       match typ with
       | 0 -> let hostname = copy buf 3 entrylen in
              (Some hostname, rt)
       | _ -> (None, rt)
     in
     let list_length = BE.get_uint16 buf 0 in
     if list_length + 2 <> n then
       raise_trailing_bytes "hostname"
     else
       parse_list parsef (sub buf 2 list_length) []

let parse_fragment_length buf =
  if len buf <> 1 then
    raise_trailing_bytes "fragment length"
  else
    int_to_max_fragment_length (get_uint8 buf 0)

let parse_supported_version buf =
  parse_any_version_opt buf

let parse_supported_versions buf =
  let len = get_uint8 buf 0 in
  if len mod 2 <> 0 then
    raise_wrong_length "supported versions"
  else
    parse_count_list parse_supported_version (shift buf 1) [] (len / 2)

let parse_named_group buf =
  let typ = BE.get_uint16 buf 0 in
  (int_to_named_group typ, shift buf 2)

let parse_group buf =
  match parse_named_group buf with
  | Some x, buf -> (named_group_to_group x, buf)
  | None, buf -> (None, buf)

let parse_supported_groups buf =
  let count = BE.get_uint16 buf 0 in
  if count mod 2 <> 0 then
    raise_wrong_length "elliptic curve list"
  else
    let cs, rt = parse_count_list parse_named_group (shift buf 2) [] (count / 2) in
    if len rt <> 0 then
      raise_trailing_bytes "elliptic curves"
    else
      cs

let parse_signature_algorithm buf =
  match int_to_signature_alg (BE.get_uint16 buf 0) with
  | Some sig_alg -> of_signature_alg sig_alg
  | _            -> None

let parse_signature_algorithms buf =
  let parsef buf = parse_signature_algorithm buf, shift buf 2 in
  let count = BE.get_uint16 buf 0 in
  if count mod 2 <> 0 then
    raise_wrong_length "signature hash"
  else
    parse_count_list parsef (shift buf 2) [] (count / 2)

let parse_alpn_protocol raw =
  let length = get_uint8 raw 0 in
  let buf = sub raw 1 length in
  let protocol = Cstruct.to_string buf in
  (Some protocol, shift raw (1 + length))

let parse_alpn_protocols buf =
  let length = BE.get_uint16 buf 0 in
  if len buf <> length + 2 then
    raise_trailing_bytes "alpn"
  else
    parse_list parse_alpn_protocol (sub buf 2 length) []

let parse_extension buf = function
  | MAX_FRAGMENT_LENGTH ->
     (match parse_fragment_length buf with
      | Some mfl -> `MaxFragmentLength mfl
      | None     -> raise_unknown "maximum fragment length")
  | RENEGOTIATION_INFO ->
       let len' = get_uint8 buf 0 in
       if len buf <> len' + 1 then
         raise_trailing_bytes "renegotiation"
       else
         `SecureRenegotiation (sub buf 1 len')
  | EXTENDED_MASTER_SECRET ->
      if len buf > 0 then
         raise_trailing_bytes "extended master secret"
      else
        `ExtendedMasterSecret
  | x -> `UnknownExtension (extension_type_to_int x, buf)

let parse_keyshare_entry buf =
  let parse_share data =
    let size = BE.get_uint16 data 0 in
    split (shift data 2) size
  in
  let g, rest = parse_named_group buf in
  let share, left = parse_share rest in
  match g with
  | None -> None, left
  | Some g -> Some (g, share), left

let parse_id buf =
  let id_len = BE.get_uint16 buf 0 in
  if id_len = 0 then (* id must be non-empty! *)
    raise_wrong_length "PSK id is empty"
  else
    let age = BE.get_uint32 buf (id_len + 2) in
    (Some (sub buf 2 id_len, age), shift buf (id_len + 6))

let parse_binder buf =
  let l = get_uint8 buf 0 in
  Some (sub buf 1 l), shift buf (l + 1)

let parse_client_presharedkeys buf =
  let id_len = BE.get_uint16 buf 0 in
  let identities = parse_list parse_id (sub buf 2 id_len) [] in
  let binders_len = BE.get_uint16 buf (id_len + 2) in
  let binders = parse_list parse_binder (sub buf (4 + id_len) binders_len) [] in
  Printf.printf "identities are %d, binders %d\n%!" (List.length identities) (List.length binders) ;
  let id_binder = List.combine identities binders in
  if len buf <> 4 + binders_len + id_len then
    raise_trailing_bytes "psk"
  else
    id_binder

let parse_cookie buf =
  let len = BE.get_uint16 buf 0 in
  (sub buf 2 len, shift buf (2 + len))

let parse_psk_key_exchange_mode buf =
  let data = get_uint8 buf 0 in
  (int_to_psk_key_exchange_mode data, shift buf 1)

let parse_psk_key_exchange_modes buf =
  let count = get_uint8 buf 0 in
  parse_count_list parse_psk_key_exchange_mode (shift buf 1) [] count

let parse_ext raw =
  let etype = BE.get_uint16 raw 0
  and length = BE.get_uint16 raw 2
  in
  (etype, length, sub raw 4 length)

let parse_client_extension raw =
  let etype, length, buf = parse_ext raw in
  let data =
    match int_to_extension_type etype with
    | Some SERVER_NAME ->
       (match parse_hostnames buf with
        | [name] -> `Hostname name
        | _      -> raise_unknown "bad server name indication (multiple names)")
    | Some SUPPORTED_GROUPS ->
       let gs = parse_supported_groups buf in
       `SupportedGroups gs
    | Some PADDING ->
       let rec check = function
         | 0 -> `Padding length
         | n -> let idx = pred n in
                if get_uint8 buf idx <> 0 then
                  raise_unknown "bad padding in padding extension"
                else
                  check idx
       in
       check length
    | Some SIGNATURE_ALGORITHMS ->
       let algos, rt = parse_signature_algorithms buf in
       if len rt <> 0 then
         raise_trailing_bytes "signature algorithms"
       else
         `SignatureAlgorithms algos
    | Some APPLICATION_LAYER_PROTOCOL_NEGOTIATION ->
      let protocols = parse_alpn_protocols buf in
      `ALPN protocols
    | Some KEY_SHARE ->
       let ll = BE.get_uint16 buf 0 in
       if ll + 2 <> len buf then
         raise_unknown "bad key share extension"
       else
         let shares = parse_list parse_keyshare_entry (sub buf 2 ll) [] in
         `KeyShare shares
    | Some PRE_SHARED_KEY ->
      let ids = parse_client_presharedkeys buf in
      `PreSharedKeys ids
    | Some EARLY_DATA ->
      if len buf <> 0 then
        raise_trailing_bytes "early data"
      else
        `EarlyDataIndication
    | Some SUPPORTED_VERSIONS ->
      let versions, rt = parse_supported_versions buf in
      if len rt <> 0 then
        raise_trailing_bytes "supported versions"
      else
        `SupportedVersions versions
    | Some POST_HANDSHAKE_AUTH ->
      if len buf = 0 then
        `PostHandshakeAuthentication
      else
        raise_unknown "non-empty post handshake authentication"
    | Some COOKIE ->
      let c, rt = parse_cookie buf in
      if len rt <> 0 then
        raise_trailing_bytes "cookie"
      else
        `Cookie c
    | Some PSK_KEY_EXCHANGE_MODES ->
      let modes, rt = parse_psk_key_exchange_modes buf in
      if len rt <> 0 then
        raise_trailing_bytes "psk key exchange modes"
      else
        `PskKeyExchangeModes modes
    | Some x -> parse_extension buf x
    | None -> `UnknownExtension (etype, buf)
  in
  (Some data, shift raw (4 + length))

let parse_server_extension raw =
  let etype, length, buf = parse_ext raw in
  let data =
    match int_to_extension_type etype with
    | Some SERVER_NAME ->
       (match parse_hostnames buf with
        | [] -> `Hostname
        | _      -> raise_unknown "bad server name indication (multiple names)")
    | Some KEY_SHARE ->
       (match parse_keyshare_entry buf with
        | _, xs when len xs <> 0 -> raise_trailing_bytes "server keyshare"
        | None, _ -> raise_unknown "keyshare entry"
        | Some (g, ks), _ ->
          match named_group_to_group g with
          | Some g -> `KeyShare (g, ks)
          | None -> raise_unknown "keyshare entry")
    | Some PRE_SHARED_KEY ->
      if len buf <> 2 then
        raise_trailing_bytes "server pre_shared_key"
      else
        `PreSharedKey (BE.get_uint16 buf 0)
    | Some SUPPORTED_GROUPS | Some SIGNATURE_ALGORITHMS | Some PADDING ->
       raise_unknown "invalid extension in server hello!"
    | Some APPLICATION_LAYER_PROTOCOL_NEGOTIATION ->
      (match parse_alpn_protocols buf with
       | [protocol] -> `ALPN protocol
       | _ -> raise_unknown "bad ALPN (none or multiple names)")
    | Some SUPPORTED_VERSIONS ->
      let version = parse_version_exn buf in
      `SelectedVersion version
    | Some x -> parse_extension buf x
    | None -> `UnknownExtension (etype, buf)
  in
  (Some data, shift raw (4 + length))

let parse_encrypted_extension raw =
  let etype, length, buf = parse_ext raw in
  let data =
    match int_to_extension_type etype with
    | Some SERVER_NAME ->
       (match parse_hostnames buf with
        | [] -> `Hostname
        | _      -> raise_unknown "bad server name indication (multiple names)")
    | Some SUPPORTED_GROUPS ->
       let gs = parse_supported_groups buf in
       let supported = Utils.filter_map ~f:named_group_to_group gs in
       `SupportedGroups supported
    | Some APPLICATION_LAYER_PROTOCOL_NEGOTIATION ->
      (match parse_alpn_protocols buf with
       | [protocol] -> `ALPN protocol
       | _ -> raise_unknown "bad ALPN (none or multiple names)")
    | Some EARLY_DATA ->
       if len buf <> 0 then
         raise_trailing_bytes "server early_data"
       else
         `EarlyDataIndication
    | Some x -> raise_unknown ("bad encrypted extension " ^ (extension_type_to_string x)) (* TODO maybe unknown instead? *)
    | None -> `UnknownExtension (etype, buf)
  in
  (Some data, shift raw (4 + length))

let parse_retry_extension raw =
  let etype, length, buf = parse_ext raw in
  let data =
    match int_to_extension_type etype with
    | Some KEY_SHARE ->
      begin
        let group, rt = parse_group buf in
        if len rt <> 0 then
          raise_trailing_bytes "key share"
        else
          match group with
          | None -> raise_unknown "unknown group in key share"
          | Some g -> `SelectedGroup g
      end
    | Some SUPPORTED_VERSIONS ->
      let version = parse_version_exn buf in
      `SelectedVersion version
    | Some COOKIE ->
      let c, rt = parse_cookie buf in
       if len rt <> 0 then
         raise_trailing_bytes "cookie"
       else
         `Cookie c
    | _ -> `UnknownExtension (etype, buf)
  in
  (Some data, shift raw (4 + length))

let parse_extensions parse_ext buf =
  let length = BE.get_uint16 buf 0 in
  if len buf <> length + 2 then
    raise_trailing_bytes "extensions"
  else
    parse_list parse_ext (sub buf 2 length) []

let parse_client_hello buf =
  let client_version = parse_any_version_exn buf in
  let client_random = sub buf 2 32 in
  let slen = get_uint8 buf 34 in
  let sessionid = if slen = 0 then None else Some (sub buf 35 slen) in
  let ciphersuites, rt = parse_any_ciphersuites (shift buf (35 + slen)) in
  let _, rt' = parse_compression_methods rt in
  let extensions =
    if len rt' == 0 then [] else parse_extensions parse_client_extension rt'
  in
  (* TLS 1.3 mandates PreSharedKeys to be the last extension *)
  (if List.exists (function `PreSharedKeys _ -> true | _ -> false) extensions then
     match List.rev extensions with
     | `PreSharedKeys _::_ -> ()
     | _ -> raise_unknown "Pre-shared key extension exists, but is not the last");
  ClientHello { client_version ; client_random ; sessionid ; ciphersuites ; extensions }

let parse_server_hello buf =
  let server_version = parse_version_exn buf in
  let server_random = sub buf 2 32 in
  let slen = get_uint8 buf 34 in
  let sessionid = if slen = 0 then None else Some (sub buf 35 slen) in
  let ciphersuite, rt = match parse_ciphersuite (shift buf (35 + slen)) with
    | Some x, buf' -> (x, buf')
    | None  , _    -> raise_unknown "ciphersuite"
  in
  let rt' = match parse_compression_method rt with
    | Some NULL, buf' -> buf'
    | Some _   , _    -> raise_unknown "unsupported compression method"
    | None     , _    -> raise_unknown "compression method"
  in
  (* depending on the content of the server_random we have to diverge in behaviour *)
  if Cstruct.equal server_random helloretryrequest then begin
    (* hello retry request, TODO: verify compression=empty *)
    match Ciphersuite.ciphersuite_to_ciphersuite13 ciphersuite with
    | None -> raise_unknown "unsupported ciphersuite in hello retry request"
    | Some ciphersuite ->
      let extensions =
        if len rt' == 0 then [] else parse_extensions parse_retry_extension rt'
      in
      let retry_version =
        match Utils.map_find ~f:(function `SelectedVersion v -> Some v | _ -> None) extensions with
        | None -> server_version
        | Some v -> v
      in
      let selected_group =
        match Utils.map_find ~f:(function `SelectedGroup g -> Some g | _ -> None) extensions with
        | None -> raise_unknown "unknown selected group"
        | Some g -> g
      in
      HelloRetryRequest { retry_version ; sessionid ; ciphersuite ; selected_group ; extensions }
  end else begin
    let extensions =
      if len rt' == 0 then [] else parse_extensions parse_server_extension rt'
    in
    let server_version =
      match Utils.map_find ~f:(function `SelectedVersion v -> Some v | _ -> None) extensions with
      | None -> server_version
      | Some v -> v
    in
    ServerHello { server_version ; server_random ; sessionid ; ciphersuite ; extensions }
  end

let parse_certificates_exn buf =
  let parsef buf =
    let len = get_uint24_len buf in
    (Some (sub buf 3 len), shift buf (len + 3))
  in
  let length = get_uint24_len buf in
  if len buf <> length + 3 then
    raise_trailing_bytes "certificates"
  else
    parse_list parsef (sub buf 3 length) []

let parse_certificates = catch @@ parse_certificates_exn

(* TODO finish implementation of certificate extensions *)
let parse_certificate_ext _ = None, Cstruct.empty

let parse_certificate_ext_1_3_exn buf =
  let certlen = get_uint24_len buf in
  let cert, extbuf, rest =
    let cert, rt = split (shift buf 3) certlen in
    let ext_len = BE.get_uint16 rt 0 in
    let extbuf, rt = split (shift rt 2) ext_len in
    cert, extbuf, rt
  in
  let exts = parse_list parse_certificate_ext extbuf [] in
  (Some (cert, exts), rest)

let parse_certificate_ext_list_1_3_exn buf =
  let length = get_uint24_len buf in
  if len buf <> length + 3 then
    raise_trailing_bytes "certificates"
  else
    parse_list parse_certificate_ext_1_3_exn (shift buf 3) []

let parse_certificates_1_3_exn buf =
  let clen = get_uint8 buf 0 in
  let context, rt = split (shift buf 1) clen in
  let certs = parse_certificate_ext_list_1_3_exn rt in
  (context, certs)

let parse_certificates_1_3 = catch @@ parse_certificates_1_3_exn

let parse_certificate_types buf =
  let parsef buf =
    let byte = get_uint8 buf 0 in
    (int_to_client_certificate_type byte, shift buf 1)
  in
  let count = get_uint8 buf 0 in
  parse_count_list parsef (shift buf 1) [] count

let parse_cas buf =
  let parsef buf =
    let length = BE.get_uint16 buf 0 in
    let name = sub buf 2 length in
    (Some name, shift buf (2 + length))
  in
  let calength = BE.get_uint16 buf 0 in
  let cas, rt = split (shift buf 2) calength in
  (parse_list parsef cas [], rt)

let parse_certificate_request_exn buf =
  let certificate_types, buf' = parse_certificate_types buf in
  let certificate_authorities, buf' = parse_cas buf' in
  if len buf' <> 0 then
    raise_trailing_bytes "certificate request"
  else
    (certificate_types, certificate_authorities)

let parse_certificate_request =
  catch parse_certificate_request_exn

let parse_certificate_request_1_2_exn buf =
  let certificate_types, buf' = parse_certificate_types buf in
  let sigs, buf' = parse_signature_algorithms buf' in
  let cas, buf' = parse_cas buf' in
  if len buf' <> 0 then
    raise_trailing_bytes "certificate request"
  else
    (certificate_types, sigs, cas)

let parse_certificate_request_1_2 =
  catch parse_certificate_request_1_2_exn

let parse_certificate_request_extension raw =
  let etype, length, buf = parse_ext raw in
  let data = match int_to_extension_type etype with
    | Some SIGNATURE_ALGORITHMS ->
      let algos, rt = parse_signature_algorithms buf in
      if len rt <> 0 then
        raise_trailing_bytes "signature algorithms"
      else
        `SignatureAlgorithms algos
    | Some CERTIFICATE_AUTHORITIES ->
      let cas, rt = parse_cas buf in
      if len rt <> 0 then
        raise_trailing_bytes "certificate authorities"
      else
        let cas = List.fold_left (fun cas buf ->
            match X509.Distinguished_name.decode_der buf with
            | Ok ca -> ca :: cas
            | Error _ -> cas)
            [] cas
        in
        `CertificateAuthorities (List.rev cas)
    | _ -> `UnknownExtension (etype, buf)
  in
  (Some data, shift raw (4 + length))

let parse_certificate_request_1_3_exn buf =
  let contextlen = get_uint8 buf 0 in
  let context, rt =
    if contextlen = 0 then
      None, shift buf 1
    else
      let ctx, rest = split (shift buf 1) contextlen in
      Some ctx, rest
  in
  let exts = parse_extensions parse_certificate_request_extension rt in
  (context, exts)

let parse_certificate_request_1_3 =
  catch parse_certificate_request_1_3_exn

let parse_dh_parameters = catch @@ fun raw ->
  let plength = BE.get_uint16 raw 0 in
  let dh_p = sub raw 2 plength in
  let buf = shift raw (2 + plength) in
  let glength = BE.get_uint16 buf 0 in
  let dh_g = sub buf 2 glength in
  let buf = shift buf (2 + glength) in
  let yslength = BE.get_uint16 buf 0 in
  let dh_Ys = sub buf 2 yslength in
  let buf = shift buf (2 + yslength) in
  let rawparams = sub raw 0 (plength + glength + yslength + 6) in
  ({ dh_p ; dh_g ; dh_Ys }, rawparams, buf)

let parse_ec_parameters = catch @@ fun raw ->
  if get_uint8 raw 0 <> ec_curve_type_to_int NAMED_CURVE then
    raise_unknown "EC curve type"
  else
    match int_to_named_group (BE.get_uint16 raw 1) with
    | Some g ->
      begin match named_group_to_group g with
        | Some ((`X25519 | `P256) as g) ->
          let data_len = get_uint8 raw 3 in
          let d, rest = split (shift raw 4) data_len in
          g, d, sub raw 0 (data_len + 4), rest
        | _ -> raise_unknown "EC group"
      end
    | None -> raise_unknown "EC named group"

let parse_digitally_signed_exn buf =
  let siglen = BE.get_uint16 buf 0 in
  if len buf <> siglen + 2 then
    raise_trailing_bytes "digitally signed"
  else
    sub buf 2 siglen

let parse_digitally_signed =
  catch parse_digitally_signed_exn

let parse_digitally_signed_1_2 = catch @@ fun buf ->
  match parse_signature_algorithm buf with
  | Some sig_alg ->
    let signature = parse_digitally_signed_exn (shift buf 2) in
    (sig_alg, signature)
  | None -> raise_unknown "hash or signature algorithm"

let parse_session_ticket_extension raw =
  let etype, length, buf = parse_ext raw in
  let data = match int_to_extension_type etype with
    | Some EARLY_DATA ->
      if len buf <> 4 then
        raise_unknown "bad early_data extension in session ticket"
      else
        let size = BE.get_uint32 buf 0 in
        `EarlyDataIndication size
    | _ -> `UnknownExtension (etype, buf)
  in
  (Some data, shift raw (4 + length))

let parse_session_ticket buf =
  let lifetime = BE.get_uint32 buf 0
  and age_add = BE.get_uint32 buf 4
  and nonce_len = get_uint8 buf 8
  in
  let nonce = sub buf 9 nonce_len in
  let ticket_len = BE.get_uint16 buf (9 + nonce_len) in
  let ticket, exts_buf = split (shift buf (11 + nonce_len)) ticket_len in
  let extensions = parse_extensions parse_session_ticket_extension exts_buf in
  { lifetime ; age_add ; nonce ; ticket ; extensions }

let parse_client_dh_key_exchange_exn buf =
  let length = BE.get_uint16 buf 0 in
  if len buf <> length + 2 then
    raise_trailing_bytes "client key exchange"
  else
    sub buf 2 length

let parse_client_dh_key_exchange = catch parse_client_dh_key_exchange_exn

let parse_client_ec_key_exchange_exn buf =
  let length = get_uint8 buf 0 in
  if len buf <> length + 1 then
    raise_trailing_bytes "client key exchange"
  else
    sub buf 1 length

let parse_client_ec_key_exchange = catch parse_client_ec_key_exchange_exn

let parse_keyupdate buf =
  if len buf <> 1 then
    raise_trailing_bytes "key update"
  else
    match int_to_key_update_request_type (get_uint8 buf 0) with
    | Some y -> y
    | None -> raise_unknown "key update content"

let parse_handshake_frame buf =
  if len buf < 4 then
    (None, buf)
  else
    let l = get_uint24_len (shift buf 1) in
    let hslen = l + 4 in
    if len buf >= hslen then
      let hs, rest = split buf hslen in
      (Some hs, rest)
    else
      (None, buf)

let parse_handshake = catch @@ fun buf ->
  let typ = get_uint8 buf 0 in
  let handshake_type = int_to_handshake_type typ in
  let length = get_uint24_len (shift buf 1) in
  if len buf <> length + 4 then
    raise_trailing_bytes "handshake"
  else
    let payload = sub buf 4 length in
    match handshake_type with
    | Some HELLO_REQUEST ->
      if len payload = 0 then HelloRequest else raise_trailing_bytes "hello request"
    | Some CLIENT_HELLO -> parse_client_hello payload
    | Some SERVER_HELLO -> parse_server_hello payload
    | Some CERTIFICATE -> Certificate payload
    | Some CERTIFICATE_VERIFY -> CertificateVerify payload
    | Some SERVER_KEY_EXCHANGE -> ServerKeyExchange payload
    | Some SERVER_HELLO_DONE ->
      if len payload = 0 then ServerHelloDone else raise_trailing_bytes "server hello done"
    | Some CERTIFICATE_REQUEST -> CertificateRequest payload
    | Some CLIENT_KEY_EXCHANGE -> ClientKeyExchange payload
    | Some FINISHED -> Finished payload
    | Some ENCRYPTED_EXTENSIONS ->
      let ee = parse_extensions parse_encrypted_extension payload in
      EncryptedExtensions ee
    | Some KEY_UPDATE ->
      let ku = parse_keyupdate payload in
      KeyUpdate ku
    | Some SESSION_TICKET ->
      let ticket = parse_session_ticket payload in
      SessionTicket ticket
    | Some END_OF_EARLY_DATA ->
      EndOfEarlyData
    | Some _
    | None  -> raise_unknown @@ "handshake type" ^ string_of_int typ
