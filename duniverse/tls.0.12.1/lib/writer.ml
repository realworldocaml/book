open Packet
open Core
open Cstruct

let (<+>) = Utils.Cs.(<+>)

let assemble_protocol_version_int buf version =
  let major, minor = pair_of_tls_version version in
  set_uint8 buf 0 major;
  set_uint8 buf 1 minor

let assemble_protocol_version version =
  let buf = create 2 in
  assemble_protocol_version_int buf version;
  buf

let assemble_any_protocol_version version =
  let buf = create 2 in
  let major, minor = pair_of_tls_any_version version in
  set_uint8 buf 0 major ;
  set_uint8 buf 1 minor ;
  buf

let assemble_hdr version (content_type, payload) =
  let buf = create 5 in
  set_uint8 buf 0 (content_type_to_int content_type);
  assemble_protocol_version_int (shift buf 1) version;
  BE.set_uint16 buf 3 (len payload);
  buf <+> payload

type len = One | Two | Three

let assemble_list ?none_if_empty lenb f elements =
  let length body =
    match lenb with
    | One   ->
       let l = create 1 in
       set_uint8 l 0 (len body) ;
       l
    | Two   ->
       let l = create 2 in
       BE.set_uint16 l 0 (len body) ;
       l
    | Three ->
       let l = create 3 in
       set_uint24_len l (len body) ;
       l
  in
  let b es = Utils.Cs.appends (List.map f es) in
  let full es =
    let body = b es in
    length body <+> body
  in
  match none_if_empty with
  | Some _ -> (match elements with
               | []   -> create 0
               | eles -> full eles)
  | None   -> full elements

let assemble_certificate c =
  let length = len c in
  let buf = create 3 in
  set_uint24_len buf length;
  buf <+> c

let assemble_certificates cs =
  assemble_list Three assemble_certificate cs

let assemble_compression_method m =
  let buf = create 1 in
  set_uint8 buf 0 (compression_method_to_int m);
  buf

let assemble_compression_methods ms =
  assemble_list One assemble_compression_method ms

let assemble_any_ciphersuite c =
  let buf = create 2 in
  BE.set_uint16 buf 0 (any_ciphersuite_to_int c);
  buf

let assemble_any_ciphersuites cs =
  assemble_list Two assemble_any_ciphersuite cs

let assemble_ciphersuite c =
  let acs = Ciphersuite.ciphersuite_to_any_ciphersuite c in
  assemble_any_ciphersuite acs

let assemble_hostname host =
  (* 8 bit hostname type; 16 bit length; value *)
  let vallength = String.length host in
  let buf = create 3 in
  set_uint8 buf 0 0; (* type, only 0 registered *)
  BE.set_uint16 buf 1 vallength;
  buf <+> (of_string host)

let assemble_hostnames hosts =
  assemble_list Two assemble_hostname hosts

let assemble_hash_signature sigalg =
  let buf = create 2 in
  BE.set_uint16 buf 0 (signature_alg_to_int (to_signature_alg sigalg)) ;
  buf

let assemble_signature_algorithms s =
  assemble_list Two assemble_hash_signature s

let assemble_certificate_types ts =
  let ass x =
    let buf = create 1 in
    set_uint8 buf 0 (client_certificate_type_to_int x) ;
    buf
  in
  assemble_list One ass ts

let assemble_cas cas =
  let ass x =
    let buf = create 2 in
    BE.set_uint16 buf 0 (len x) ;
    buf <+> x
  in
  assemble_list Two ass cas

let assemble_certificate_request ts cas =
  assemble_certificate_types ts <+> assemble_cas cas

let assemble_certificate_request_1_2 ts sigalgs cas =
  assemble_certificate_types ts <+>
    assemble_signature_algorithms sigalgs <+>
    assemble_cas cas

let assemble_named_group g =
  let buf = create 2 in
  BE.set_uint16 buf 0 (named_group_to_int g);
  buf

let assemble_group g =
  assemble_named_group (group_to_named_group g)

let assemble_supported_groups groups =
  assemble_list Two assemble_named_group groups

let assemble_keyshare_entry (ng, ks) =
  let g = assemble_named_group ng in
  let l = create 2 in
  BE.set_uint16 l 0 (len ks) ;
  g <+> l <+> ks

let assemble_psk_id (id, age) =
  let id_len = create 2 in
  BE.set_uint16 id_len 0 (len id) ;
  let age_buf = create 4 in
  BE.set_uint32 age_buf 0 age ;
  id_len <+> id <+> age_buf

let assemble_binder b =
  let b_len = create 1 in
  set_uint8 b_len 0 (len b) ;
  b_len <+> b

let assemble_client_psks psks =
  let ids, binders = List.split psks in
  let ids_buf = assemble_list Two assemble_psk_id ids in
  let binders_buf = assemble_list Two assemble_binder binders in
  ids_buf <+> binders_buf

let assemble_alpn_protocol p =
  let buf = create 1 in
  set_uint8 buf 0 (String.length p) ;
  buf <+> Cstruct.of_string p

let assemble_alpn_protocols protocols =
  assemble_list Two assemble_alpn_protocol protocols

let assemble_supported_versions vs =
  assemble_list One assemble_any_protocol_version vs

let assemble_extension = function
  | `SecureRenegotiation x ->
     let buf = create 1 in
     set_uint8 buf 0 (len x);
     (buf <+> x, RENEGOTIATION_INFO)
  | `ExtendedMasterSecret -> (create 0, EXTENDED_MASTER_SECRET)
  | _ -> invalid_arg "unknown extension"

let assemble_cookie c =
  let l = create 2 in
  BE.set_uint16 l 0 (len c) ;
  l <+> c

let assemble_psk_key_exchange_mode mode =
  let c = create 1 in
  set_uint8 c 0 (psk_key_exchange_mode_to_int mode) ;
  c

let assemble_psk_key_exchange_modes modes =
  assemble_list One assemble_psk_key_exchange_mode modes

let assemble_ext (pay, typ) =
  let buf = Cstruct.create 4 in
  BE.set_uint16 buf 0 (extension_type_to_int typ);
  BE.set_uint16 buf 2 (len pay);
  buf <+> pay

let assemble_extensions ?none_if_empty assemble_e es =
  assemble_list ?none_if_empty Two assemble_e es

let assemble_ca ca =
  let lenbuf = create 2 in
  let data = X509.Distinguished_name.encode_der ca in
  BE.set_uint16 lenbuf 0 (len data) ;
  lenbuf <+> data

let assemble_certificate_authorities cas =
  assemble_list Two assemble_ca cas

let assemble_certificate_request_extension e =
  assemble_ext @@ match e with
  | `SignatureAlgorithms s ->
    (assemble_signature_algorithms s, SIGNATURE_ALGORITHMS)
  | `CertificateAuthorities cas ->
    (assemble_certificate_authorities cas, CERTIFICATE_AUTHORITIES)
  | _ -> invalid_arg "unknown extension"

let assemble_certificate_request_1_3 ?(context = Cstruct.empty) exts =
  let clen = create 1 in
  set_uint8 clen 0 (len context) ;
  let exts = assemble_extensions assemble_certificate_request_extension exts in
  clen <+> context <+> exts

let assemble_client_extension e =
  assemble_ext @@ match e with
    | `SupportedGroups groups ->
      (assemble_supported_groups groups, SUPPORTED_GROUPS)
    | `Hostname name -> (assemble_hostnames [name], SERVER_NAME)
    | `Padding x -> (create x, PADDING)
    | `SignatureAlgorithms s ->
      (assemble_signature_algorithms s, SIGNATURE_ALGORITHMS)
    | `ALPN protocols ->
      (assemble_alpn_protocols protocols, APPLICATION_LAYER_PROTOCOL_NEGOTIATION)
    | `KeyShare ks ->
      (assemble_list Two assemble_keyshare_entry ks, KEY_SHARE)
    | `PreSharedKeys ids ->
      (assemble_client_psks ids, PRE_SHARED_KEY)
    | `EarlyDataIndication ->
      (create 0, EARLY_DATA)
    | `SupportedVersions vs ->
      (assemble_supported_versions vs, SUPPORTED_VERSIONS)
    | `PostHandshakeAuthentication ->
      (Utils.Cs.empty, POST_HANDSHAKE_AUTH)
    | `Cookie c ->
      (assemble_cookie c, COOKIE)
    | `PskKeyExchangeModes modes ->
      (assemble_psk_key_exchange_modes modes, PSK_KEY_EXCHANGE_MODES)
    | x -> assemble_extension x

let assemble_server_extension e =
  assemble_ext @@ match e with
    | `Hostname -> (create 0, SERVER_NAME)
    | `ALPN protocol ->
      (assemble_alpn_protocols [protocol], APPLICATION_LAYER_PROTOCOL_NEGOTIATION)
    | `KeyShare (g, ks) ->
      let ng = group_to_named_group g in
      (assemble_keyshare_entry (ng, ks), KEY_SHARE)
    | `PreSharedKey id ->
      let data = create 2 in
      BE.set_uint16 data 0 id ;
      (data, PRE_SHARED_KEY)
    | `SelectedVersion v -> (assemble_protocol_version v, SUPPORTED_VERSIONS)
    | x -> assemble_extension x

let assemble_encrypted_extension e =
  assemble_ext @@ match e with
    | `Hostname -> (create 0, SERVER_NAME)
    | `ALPN protocol ->
      (assemble_alpn_protocols [protocol], APPLICATION_LAYER_PROTOCOL_NEGOTIATION)
    | `SupportedGroups groups ->
      (assemble_supported_groups (List.map group_to_named_group groups), SUPPORTED_GROUPS)
    | `EarlyDataIndication -> (create 0, EARLY_DATA)
    | _ -> invalid_arg "unknown extension"

let assemble_retry_extension e =
  assemble_ext @@ match e with
    | `SelectedGroup g -> (assemble_group g, KEY_SHARE)
    | `Cookie c -> (assemble_cookie c, COOKIE)
    | `SelectedVersion v -> (assemble_protocol_version v, SUPPORTED_VERSIONS)
    | `UnknownExtension _ -> invalid_arg "unknown retry extension"

let assemble_cert_ext (certificate, extensions) =
  let cert = assemble_certificate certificate
  and exts = assemble_list Two assemble_server_extension extensions
  in
  cert <+> exts

let assemble_certs_exts cs =
  assemble_list Three assemble_cert_ext cs

let assemble_certificates_1_3 context certs =
  let l = create 1 in
  set_uint8 l 0 (len context) ;
  l <+> context <+> assemble_certs_exts (List.map (fun c -> c, []) certs)

let assemble_sid sid =
  let buf = create 1 in
  match sid with
  | None   -> buf
  | Some s -> set_uint8 buf 0 (len s); buf <+> s

let assemble_client_hello (cl : client_hello) : Cstruct.t =
  let version = match cl.client_version with
    | `TLS_1_3 -> `TLS_1_2 (* keep 0x03 0x03 on wire *)
    | x -> x
  in
  let v = assemble_any_protocol_version version in
  let sid = assemble_sid cl.sessionid in
  let css = assemble_any_ciphersuites cl.ciphersuites in
  (* compression methods, completely useless *)
  let cms = assemble_compression_methods [NULL] in
  let bbuf = v <+> cl.client_random <+> sid <+> css <+> cms in
  let extensions = assemble_extensions ~none_if_empty:true assemble_client_extension cl.extensions in
  (* some widely deployed firewalls drop ClientHello messages which are
     > 256 and < 511 byte, insert PADDING extension for these *)
  (* from draft-ietf-tls-padding-00:
   As an example, consider a client that wishes to avoid sending a
   ClientHello with a record size between 256 and 511 bytes (inclusive).
   This case is considered because at least one TLS implementation is
   known to hang the connection when such a ClientHello record is
   received.

   After building a ClientHello as normal, the client can add four to
   the length (to account for the "msg_type" and "length" fields of the
   handshake protocol) and test whether the resulting length falls into
   that range.  If it does, a padding extension can be added in order to
   push the length to (at least) 512 bytes. *)
  let extrapadding =
    (* since PreSharedKeys _must_ be the last extension, don't bother padding
       when it is present. rationale from ietf-tls WG
       "Padding extension and 0-RTT" thread (2016-10-30) *)
    if List.exists (function `PreSharedKeys _ -> true | _ -> false) cl.extensions then
      Cstruct.empty
    else
      let buflen = len bbuf + len extensions + 4 (* see above, header *) in
      if buflen >= 256 && buflen <= 511 then
        match len extensions with
        | 0 -> (* need to construct a 2 byte extension length as well *)
          let l = 512 (* desired length *) - 2 (* extension length *) - 4 (* padding extension header *) - buflen in
          let l = max l 0 in (* negative size is not good *)
          let padding = assemble_client_extension (`Padding l) in
          let extension_length = create 2 in
          BE.set_uint16 extension_length 0 (len padding);
          extension_length <+> padding
        | _ ->
          let l = 512 - 4 (* padding extension header *) - buflen in
          let l = max l 0 in
          let padding = assemble_client_extension (`Padding l) in
          (* extensions include the 16 bit extension length field *)
          let elen = len extensions + len padding - 2 (* the 16 bit length field *) in
          BE.set_uint16 extensions 0 elen;
          padding
      else
        create 0
  in
  bbuf <+> extensions <+> extrapadding

let assemble_server_hello (sh : server_hello) : Cstruct.t =
  let version, exts = match sh.server_version with
    | `TLS_1_3 -> `TLS_1_2, `SelectedVersion `TLS_1_3 :: sh.extensions
    | x -> x, sh.extensions
  in
  let v = assemble_protocol_version version in
  let sid = assemble_sid sh.sessionid in
  let cs = assemble_ciphersuite sh.ciphersuite in
  (* useless compression method *)
  let cm = assemble_compression_method NULL in
  let extensions = assemble_extensions ~none_if_empty:true assemble_server_extension exts in
  v <+> sh.server_random <+> sid <+> cs <+> cm <+> extensions

let assemble_dh_parameters p =
  let plen, glen, yslen = (len p.dh_p, len p.dh_g, len p.dh_Ys) in
  let buf = create (2 + 2 + 2 + plen + glen + yslen) in
  BE.set_uint16  buf  0 plen;
  blit p.dh_p  0 buf  2 plen;
  BE.set_uint16  buf (2 + plen) glen;
  blit p.dh_g  0 buf (4 + plen) glen;
  BE.set_uint16  buf (4 + plen + glen) yslen;
  blit p.dh_Ys 0 buf (6 + plen + glen) yslen;
  buf

let assemble_ec_parameters named_curve point =
  let hdr = create 4 in
  set_uint8 hdr 0 (ec_curve_type_to_int NAMED_CURVE);
  BE.set_uint16 hdr 1 (named_group_to_int (group_to_named_group named_curve));
  set_uint8 hdr 3 (len point);
  hdr <+> point

let assemble_digitally_signed signature =
  let lenbuf = create 2 in
  BE.set_uint16 lenbuf 0 (len signature);
  lenbuf <+> signature

let assemble_digitally_signed_1_2 sigalg signature =
  (assemble_hash_signature sigalg) <+>
    (assemble_digitally_signed signature)

let assemble_session_ticket_extension e =
  assemble_ext @@ match e with
  | `EarlyDataIndication max ->
    let buf = create 4 in
    BE.set_uint32 buf 0 max ;
    (buf, EARLY_DATA)
  | _ -> invalid_arg "unknown extension"

let assemble_session_ticket (se : session_ticket) =
  let buf = create 9 in
  BE.set_uint32 buf 0 se.lifetime ;
  BE.set_uint32 buf 4 se.age_add ;
  set_uint8 buf 8 (len se.nonce) ;
  let ticketlen = create 2 in
  BE.set_uint16 ticketlen 0 (len se.ticket) ;
  let exts = assemble_extensions assemble_session_ticket_extension se.extensions in
  buf <+> se.nonce <+> ticketlen <+> se.ticket <+> exts

let assemble_client_dh_key_exchange kex =
  let len = len kex in
  let buf = create (len + 2) in
  BE.set_uint16 buf 0 len;
  blit kex 0 buf 2 len;
  buf

let assemble_client_ec_key_exchange kex =
  let len = len kex in
  let buf = create (len + 1) in
  set_uint8 buf 0 len;
  blit kex 0 buf 1 len;
  buf

let assemble_hello_retry_request hrr =
  let exts = `SelectedGroup hrr.selected_group :: hrr.extensions in
  let version, exts = match hrr.retry_version with
    | `TLS_1_3 -> `TLS_1_2, `SelectedVersion `TLS_1_3 :: exts
    | x -> x, exts
  in
  let v = assemble_protocol_version version in
  let sid = assemble_sid hrr.sessionid in
  let cs = assemble_ciphersuite (hrr.ciphersuite :> Ciphersuite.ciphersuite) in
  (* useless compression method *)
  let cm = create 1 in
  let extensions = assemble_extensions ~none_if_empty:true assemble_retry_extension exts in
  v <+> helloretryrequest <+> sid <+> cs <+> cm <+> extensions

let assemble_hs typ len =
  let buf = create 4 in
  set_uint8 buf 0 (handshake_type_to_int typ);
  set_uint24_len (shift buf 1) len;
  buf

let assemble_message_hash len =
  assemble_hs MESSAGE_HASH len

let assemble_key_update req =
  let cs = create 1 in
  set_uint8 cs 0 (key_update_request_type_to_int req);
  cs

let assemble_handshake hs =
  let (payload, payload_type) =
    match hs with
    | ClientHello ch -> (assemble_client_hello ch, CLIENT_HELLO)
    | ServerHello sh -> (assemble_server_hello sh, SERVER_HELLO)
    | HelloRetryRequest hr -> (assemble_hello_retry_request hr, SERVER_HELLO)
    | Certificate cs -> (cs, CERTIFICATE)
    | CertificateRequest cr -> (cr, CERTIFICATE_REQUEST)
    | CertificateVerify c -> (c, CERTIFICATE_VERIFY)
    | ServerKeyExchange kex -> (kex, SERVER_KEY_EXCHANGE)
    | ClientKeyExchange kex -> (kex, CLIENT_KEY_EXCHANGE)
    | ServerHelloDone -> (create 0, SERVER_HELLO_DONE)
    | HelloRequest -> (create 0, HELLO_REQUEST)
    | Finished fs -> (fs, FINISHED)
    | SessionTicket st -> (assemble_session_ticket st, SESSION_TICKET)
    | EncryptedExtensions ee ->
       let cs = assemble_extensions assemble_encrypted_extension ee in
       (cs, ENCRYPTED_EXTENSIONS)
    | KeyUpdate req ->
      let cs = assemble_key_update req in
      (cs, KEY_UPDATE)
    | EndOfEarlyData -> (create 0, END_OF_EARLY_DATA)
  in
  let pay_len = len payload in
  let buf = assemble_hs payload_type pay_len in
  buf <+> payload

let assemble_alert ?(level = Packet.FATAL) typ =
  let buf = create 2 in
  set_uint8 buf 1 (alert_type_to_int typ);
  set_uint8 buf 0 (alert_level_to_int level) ;
  buf

let assemble_change_cipher_spec =
  let ccs = create 1 in
  set_uint8 ccs 0 1;
  ccs
