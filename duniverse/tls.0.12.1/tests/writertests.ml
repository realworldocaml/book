open OUnit2
open Tls
open Testlib

let version_assembler (ver, res) _ =
  let buf = Writer.assemble_protocol_version ver in
  assert_cs_eq buf res

let version_assembler_tests = [
  (`TLS_1_0, list_to_cstruct [3; 1]) ;
  (`TLS_1_1, list_to_cstruct [3; 2]) ;
  (`TLS_1_2, list_to_cstruct [3; 3]) ;
]

let version_tests =
  List.mapi
    (fun i f -> "Assemble version " ^ string_of_int i >:: version_assembler f)
    version_assembler_tests

let hdr_assembler (ver, ct, cs, res) _ =
  let buf = Writer.assemble_hdr ver (ct, (list_to_cstruct cs)) in
  let res' = list_to_cstruct res in
  assert_cs_eq buf res'

let hdr_assembler_tests = [
  (`TLS_1_2, Packet.CHANGE_CIPHER_SPEC, [], [20; 3; 3; 0; 0]) ;
  (`TLS_1_1, Packet.CHANGE_CIPHER_SPEC, [], [20; 3; 2; 0; 0]) ;
  (`TLS_1_0, Packet.CHANGE_CIPHER_SPEC, [], [20; 3; 1; 0; 0]) ;
  (`TLS_1_2, Packet.CHANGE_CIPHER_SPEC, [0; 0; 0], [20; 3; 3; 0; 3; 0; 0; 0]) ;

  (`TLS_1_2, Packet.ALERT, [], [21; 3; 3; 0; 0]) ;
  (`TLS_1_1, Packet.ALERT, [], [21; 3; 2; 0; 0]) ;
  (`TLS_1_0, Packet.ALERT, [], [21; 3; 1; 0; 0]) ;
  (`TLS_1_2, Packet.ALERT, [0; 0; 0], [21; 3; 3; 0; 3; 0; 0; 0]) ;

  (`TLS_1_2, Packet.HANDSHAKE, [], [22; 3; 3; 0; 0]) ;
  (`TLS_1_1, Packet.HANDSHAKE, [], [22; 3; 2; 0; 0]) ;
  (`TLS_1_0, Packet.HANDSHAKE, [], [22; 3; 1; 0; 0]) ;
  (`TLS_1_2, Packet.HANDSHAKE, [0; 0; 0], [22; 3; 3; 0; 3; 0; 0; 0]) ;

  (`TLS_1_2, Packet.APPLICATION_DATA, [], [23; 3; 3; 0; 0]) ;
  (`TLS_1_1, Packet.APPLICATION_DATA, [], [23; 3; 2; 0; 0]) ;
  (`TLS_1_0, Packet.APPLICATION_DATA, [], [23; 3; 1; 0; 0]) ;
  (`TLS_1_2, Packet.APPLICATION_DATA, [0; 0; 0], [23; 3; 3; 0; 3; 0; 0; 0]) ;

  (`TLS_1_2, Packet.HEARTBEAT, [], [24; 3; 3; 0; 0]) ;
  (`TLS_1_1, Packet.HEARTBEAT, [], [24; 3; 2; 0; 0]) ;
  (`TLS_1_0, Packet.HEARTBEAT, [], [24; 3; 1; 0; 0]) ;
  (`TLS_1_2, Packet.HEARTBEAT, [0; 0; 0], [24; 3; 3; 0; 3; 0; 0; 0]) ;
]

let hdr_tests =
  List.mapi
    (fun i f -> "Assemble header " ^ string_of_int i >:: hdr_assembler f)
    hdr_assembler_tests

let alert_assembler (level, t, res) _ =
  let buf = match level with
    | None   -> Writer.assemble_alert t
    | Some l -> Writer.assemble_alert ~level:l t
  in
  let res' = list_to_cstruct res in
  assert_cs_eq buf res'

let alert_assembler_tests = Packet.([
  ( None,  CLOSE_NOTIFY                    , [ 2 ; 0;   ] ) ;
  ( None,  UNEXPECTED_MESSAGE              , [ 2 ; 10;  ] ) ;
  ( None,  BAD_RECORD_MAC                  , [ 2 ; 20;  ] ) ;
  ( None,  DECRYPTION_FAILED               , [ 2 ; 21;  ] ) ;
  ( None,  RECORD_OVERFLOW                 , [ 2 ; 22;  ] ) ;
  ( None,  DECOMPRESSION_FAILURE           , [ 2 ; 30;  ] ) ;
  ( None,  HANDSHAKE_FAILURE               , [ 2 ; 40;  ] ) ;
  ( None,  NO_CERTIFICATE_RESERVED         , [ 2 ; 41;  ] ) ;
  ( None,  BAD_CERTIFICATE                 , [ 2 ; 42;  ] ) ;
  ( None,  UNSUPPORTED_CERTIFICATE         , [ 2 ; 43;  ] ) ;
  ( None,  CERTIFICATE_REVOKED             , [ 2 ; 44;  ] ) ;
  ( None,  CERTIFICATE_EXPIRED             , [ 2 ; 45;  ] ) ;
  ( None,  CERTIFICATE_UNKNOWN             , [ 2 ; 46;  ] ) ;
  ( None,  ILLEGAL_PARAMETER               , [ 2 ; 47;  ] ) ;
  ( None,  UNKNOWN_CA                      , [ 2 ; 48;  ] ) ;
  ( None,  ACCESS_DENIED                   , [ 2 ; 49;  ] ) ;
  ( None,  DECODE_ERROR                    , [ 2 ; 50;  ] ) ;
  ( None,  DECRYPT_ERROR                   , [ 2 ; 51;  ] ) ;
  ( None,  EXPORT_RESTRICTION_RESERVED     , [ 2 ; 60;  ] ) ;
  ( None,  PROTOCOL_VERSION                , [ 2 ; 70;  ] ) ;
  ( None,  INSUFFICIENT_SECURITY           , [ 2 ; 71;  ] ) ;
  ( None,  INTERNAL_ERROR                  , [ 2 ; 80;  ] ) ;
  ( None,  USER_CANCELED                   , [ 2 ; 90;  ] ) ;
  ( None,  NO_RENEGOTIATION                , [ 2 ; 100; ] ) ;
  ( None,  UNSUPPORTED_EXTENSION           , [ 2 ; 110; ] ) ;
  ( None,  CERTIFICATE_UNOBTAINABLE        , [ 2 ; 111; ] ) ;
  ( None,  UNRECOGNIZED_NAME               , [ 2 ; 112; ] ) ;
  ( None,  BAD_CERTIFICATE_STATUS_RESPONSE , [ 2 ; 113; ] ) ;
  ( None,  BAD_CERTIFICATE_HASH_VALUE      , [ 2 ; 114; ] ) ;
  ( None,  UNKNOWN_PSK_IDENTITY            , [ 2 ; 115; ] ) ;
  ( None,  NO_APPLICATION_PROTOCOL         , [ 2 ; 120; ] ) ;

  ( Some FATAL,  CLOSE_NOTIFY                    , [ 2 ; 0;   ] ) ;
  ( Some FATAL,  UNEXPECTED_MESSAGE              , [ 2 ; 10;  ] ) ;
  ( Some FATAL,  BAD_RECORD_MAC                  , [ 2 ; 20;  ] ) ;
  ( Some FATAL,  DECRYPTION_FAILED               , [ 2 ; 21;  ] ) ;
  ( Some FATAL,  RECORD_OVERFLOW                 , [ 2 ; 22;  ] ) ;
  ( Some FATAL,  DECOMPRESSION_FAILURE           , [ 2 ; 30;  ] ) ;
  ( Some FATAL,  HANDSHAKE_FAILURE               , [ 2 ; 40;  ] ) ;
  ( Some FATAL,  NO_CERTIFICATE_RESERVED         , [ 2 ; 41;  ] ) ;
  ( Some FATAL,  BAD_CERTIFICATE                 , [ 2 ; 42;  ] ) ;
  ( Some FATAL,  UNSUPPORTED_CERTIFICATE         , [ 2 ; 43;  ] ) ;
  ( Some FATAL,  CERTIFICATE_REVOKED             , [ 2 ; 44;  ] ) ;
  ( Some FATAL,  CERTIFICATE_EXPIRED             , [ 2 ; 45;  ] ) ;
  ( Some FATAL,  CERTIFICATE_UNKNOWN             , [ 2 ; 46;  ] ) ;
  ( Some FATAL,  ILLEGAL_PARAMETER               , [ 2 ; 47;  ] ) ;
  ( Some FATAL,  UNKNOWN_CA                      , [ 2 ; 48;  ] ) ;
  ( Some FATAL,  ACCESS_DENIED                   , [ 2 ; 49;  ] ) ;
  ( Some FATAL,  DECODE_ERROR                    , [ 2 ; 50;  ] ) ;
  ( Some FATAL,  DECRYPT_ERROR                   , [ 2 ; 51;  ] ) ;
  ( Some FATAL,  EXPORT_RESTRICTION_RESERVED     , [ 2 ; 60;  ] ) ;
  ( Some FATAL,  PROTOCOL_VERSION                , [ 2 ; 70;  ] ) ;
  ( Some FATAL,  INSUFFICIENT_SECURITY           , [ 2 ; 71;  ] ) ;
  ( Some FATAL,  INTERNAL_ERROR                  , [ 2 ; 80;  ] ) ;
  ( Some FATAL,  USER_CANCELED                   , [ 2 ; 90;  ] ) ;
  ( Some FATAL,  NO_RENEGOTIATION                , [ 2 ; 100; ] ) ;
  ( Some FATAL,  UNSUPPORTED_EXTENSION           , [ 2 ; 110; ] ) ;
  ( Some FATAL,  CERTIFICATE_UNOBTAINABLE        , [ 2 ; 111; ] ) ;
  ( Some FATAL,  UNRECOGNIZED_NAME               , [ 2 ; 112; ] ) ;
  ( Some FATAL,  BAD_CERTIFICATE_STATUS_RESPONSE , [ 2 ; 113; ] ) ;
  ( Some FATAL,  BAD_CERTIFICATE_HASH_VALUE      , [ 2 ; 114; ] ) ;
  ( Some FATAL,  UNKNOWN_PSK_IDENTITY            , [ 2 ; 115; ] ) ;
  ( Some FATAL,  NO_APPLICATION_PROTOCOL         , [ 2 ; 120; ] ) ;

  ( Some WARNING,  CLOSE_NOTIFY                    , [ 1 ; 0;   ] ) ;
  ( Some WARNING,  UNEXPECTED_MESSAGE              , [ 1 ; 10;  ] ) ;
  ( Some WARNING,  BAD_RECORD_MAC                  , [ 1 ; 20;  ] ) ;
  ( Some WARNING,  DECRYPTION_FAILED               , [ 1 ; 21;  ] ) ;
  ( Some WARNING,  RECORD_OVERFLOW                 , [ 1 ; 22;  ] ) ;
  ( Some WARNING,  DECOMPRESSION_FAILURE           , [ 1 ; 30;  ] ) ;
  ( Some WARNING,  HANDSHAKE_FAILURE               , [ 1 ; 40;  ] ) ;
  ( Some WARNING,  NO_CERTIFICATE_RESERVED         , [ 1 ; 41;  ] ) ;
  ( Some WARNING,  BAD_CERTIFICATE                 , [ 1 ; 42;  ] ) ;
  ( Some WARNING,  UNSUPPORTED_CERTIFICATE         , [ 1 ; 43;  ] ) ;
  ( Some WARNING,  CERTIFICATE_REVOKED             , [ 1 ; 44;  ] ) ;
  ( Some WARNING,  CERTIFICATE_EXPIRED             , [ 1 ; 45;  ] ) ;
  ( Some WARNING,  CERTIFICATE_UNKNOWN             , [ 1 ; 46;  ] ) ;
  ( Some WARNING,  ILLEGAL_PARAMETER               , [ 1 ; 47;  ] ) ;
  ( Some WARNING,  UNKNOWN_CA                      , [ 1 ; 48;  ] ) ;
  ( Some WARNING,  ACCESS_DENIED                   , [ 1 ; 49;  ] ) ;
  ( Some WARNING,  DECODE_ERROR                    , [ 1 ; 50;  ] ) ;
  ( Some WARNING,  DECRYPT_ERROR                   , [ 1 ; 51;  ] ) ;
  ( Some WARNING,  EXPORT_RESTRICTION_RESERVED     , [ 1 ; 60;  ] ) ;
  ( Some WARNING,  PROTOCOL_VERSION                , [ 1 ; 70;  ] ) ;
  ( Some WARNING,  INSUFFICIENT_SECURITY           , [ 1 ; 71;  ] ) ;
  ( Some WARNING,  INTERNAL_ERROR                  , [ 1 ; 80;  ] ) ;
  ( Some WARNING,  USER_CANCELED                   , [ 1 ; 90;  ] ) ;
  ( Some WARNING,  NO_RENEGOTIATION                , [ 1 ; 100; ] ) ;
  ( Some WARNING,  UNSUPPORTED_EXTENSION           , [ 1 ; 110; ] ) ;
  ( Some WARNING,  CERTIFICATE_UNOBTAINABLE        , [ 1 ; 111; ] ) ;
  ( Some WARNING,  UNRECOGNIZED_NAME               , [ 1 ; 112; ] ) ;
  ( Some WARNING,  BAD_CERTIFICATE_STATUS_RESPONSE , [ 1 ; 113; ] ) ;
  ( Some WARNING,  BAD_CERTIFICATE_HASH_VALUE      , [ 1 ; 114; ] ) ;
  ( Some WARNING,  UNKNOWN_PSK_IDENTITY            , [ 1 ; 115; ] ) ;
])

let alert_tests =
  List.mapi
    (fun i f -> "Assemble alert " ^ string_of_int i >:: alert_assembler f)
    alert_assembler_tests

let ccs_test _ =
  let buf = Writer.assemble_change_cipher_spec in
  assert_cs_eq buf (list_to_cstruct [1])

let dh_assembler (p, res) _ =
  let buf = Writer.assemble_dh_parameters p in
  assert_cs_eq buf res

let dh_assembler_tests =
  let a = list_to_cstruct [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15 ] in
  let le = list_to_cstruct [ 0; 16 ] in
  let le2 = list_to_cstruct [ 0; 32 ] in
  let emp, empl = (list_to_cstruct [], list_to_cstruct [ 0; 0 ]) in
  Core.([
    ( { dh_p = a ; dh_g = a ; dh_Ys = a },
      le <+> a <+> le <+> a <+> le <+> a ) ;
    ( { dh_p = a <+> a ; dh_g = a ; dh_Ys = a <+> a },
      le2 <+> a <+> a <+> le <+> a <+> le2 <+> a <+> a ) ;
    ( { dh_p = emp ; dh_g = emp ; dh_Ys = emp }, empl <+> empl <+> empl ) ;
    ( { dh_p = a ; dh_g = emp ; dh_Ys = emp }, le <+> a <+> empl <+> empl ) ;
    ( { dh_p = emp ; dh_g = a ; dh_Ys = emp }, empl <+> le <+> a <+> empl ) ;
    ( { dh_p = emp ; dh_g = emp ; dh_Ys = a }, empl <+> empl <+> le <+> a ) ;
    ( { dh_p = emp ; dh_g = a ; dh_Ys = a }, empl <+> le <+> a <+> le <+> a ) ;
    ( { dh_p = a ; dh_g = a ; dh_Ys = emp }, le <+> a <+> le <+> a <+> empl ) ;
    ( { dh_p = a ; dh_g = emp ; dh_Ys = a }, le <+> a <+> empl <+> le <+> a ) ;
       ])

let dh_tests =
  List.mapi
    (fun i f -> "Assemble dh parameters " ^ string_of_int i >:: dh_assembler f)
    dh_assembler_tests


let ds_assembler (p, res) _ =
  let buf = Writer.assemble_digitally_signed p in
  assert_cs_eq buf res

let ds_assembler_tests =
  let a = list_to_cstruct [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15 ] in
  let le = list_to_cstruct [ 0; 16 ] in
  let le2 = list_to_cstruct [ 0; 32 ] in
  let emp, empl = (list_to_cstruct [], list_to_cstruct [ 0; 0 ]) in
  [
    ( a , le <+> a ) ;
    ( a <+> a , le2 <+> a <+> a ) ;
    ( emp , empl )
  ]

let ds_tests =
  List.mapi
    (fun i f -> "Assemble digitally signed " ^ string_of_int i >:: ds_assembler f)
    ds_assembler_tests

let ds_1_2_assembler (sigalg, p, res) _ =
  let buf = Writer.assemble_digitally_signed_1_2 sigalg p in
  assert_cs_eq buf res

let ds_1_2_assembler_tests =
  let a = list_to_cstruct [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15 ] in
  let le = list_to_cstruct [ 0; 16 ] in
  let le2 = list_to_cstruct [ 0; 32 ] in
  let emp, empl = (list_to_cstruct [], list_to_cstruct [0; 0]) in
  [
    ( `RSA_PKCS1_MD5, a , list_to_cstruct [1; 1] <+> le <+> a ) ;
    ( `RSA_PKCS1_SHA1, a <+> a , list_to_cstruct [2; 1] <+> le2 <+> a <+> a ) ;
    ( `RSA_PSS_RSAENC_SHA256, emp , list_to_cstruct [8; 4] <+> empl )
  ]

let ds_1_2_tests =
  List.mapi
    (fun i f -> "Assemble digitally signed 1.2 " ^ string_of_int i >:: ds_1_2_assembler f)
    ds_1_2_assembler_tests

let handshake_assembler (h, res) _ =
  let res' = list_to_cstruct res in
  let buf = Writer.assemble_handshake h in
  assert_cs_eq buf res'

let handshake_assembler_tests =
  let a_l = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15 ] in
  let a_cs = list_to_cstruct a_l in
  let le = [ 0; 0; 16 ] in
  let le2 = [ 0; 0; 32 ] in
  let emp, empl = (list_to_cstruct [], [ 0; 0; 0 ]) in
  Core.([
   ( HelloRequest , [ 0; 0; 0; 0 ]) ;
   ( ServerHelloDone , [ 14; 0; 0; 0 ]) ;

   ( Finished a_cs , [ 20 ] @ le @ a_l ) ;
   ( Finished emp , [ 20 ] @ empl ) ;
   ( Finished (a_cs <+> a_cs) , [ 20 ] @ le2 @ a_l @ a_l ) ;

   ( ClientKeyExchange emp , [ 16; 0; 0; 0 ] ) ;
   ( ClientKeyExchange a_cs , [ 16; 0; 0; 16 ] @ a_l ) ;
   ( ClientKeyExchange (a_cs <+> a_cs) , [ 16; 0; 0; 32 ] @ a_l @ a_l ) ;

   ( ServerKeyExchange emp , [ 12 ] @ empl ) ;
   ( ServerKeyExchange a_cs , [ 12 ] @ le @ a_l ) ;
   ( ServerKeyExchange (a_cs <+> a_cs) , [ 12 ] @ le2 @ a_l @ a_l ) ;

   ( Certificate (Writer.assemble_certificates []) , [ 11; 0; 0; 3; 0; 0; 0 ] ) ;
   ( Certificate (Writer.assemble_certificates[emp]) , [ 11; 0; 0; 6; 0; 0; 3; 0; 0; 0 ] ) ;
   ( Certificate (Writer.assemble_certificates[emp ; emp]) , [ 11; 0; 0; 9; 0; 0; 6; 0; 0; 0; 0; 0; 0 ] ) ;

   ( Certificate (Writer.assemble_certificates[a_cs]) , [ 11; 0; 0; 22; 0; 0; 19 ] @ le @ a_l ) ;
   ( Certificate (Writer.assemble_certificates[a_cs ; emp]) , [ 11; 0; 0; 25; 0; 0; 22 ] @ le @ a_l @ [ 0; 0; 0 ] ) ;
   ( Certificate (Writer.assemble_certificates[emp ; a_cs]) , [ 11; 0; 0; 25; 0; 0; 22; 0; 0; 0] @ le @ a_l ) ;
   ( Certificate (Writer.assemble_certificates[emp ; a_cs ; emp]) , [ 11; 0; 0; 28; 0; 0; 25; 0; 0; 0 ] @ le @ a_l @ [ 0; 0; 0 ]) ;
   ( Certificate (Writer.assemble_certificates[a_cs ; emp ; a_cs]) , [ 11; 0; 0; 44; 0; 0; 41 ] @ le @ a_l @ [ 0; 0; 0 ] @ le @ a_l ) ;
   ( Certificate (Writer.assemble_certificates[a_cs ; emp ; a_cs ; emp]) , [ 11; 0; 0; 47; 0; 0; 44 ] @ le @ a_l @ [ 0; 0; 0 ] @ le @ a_l @ [ 0; 0; 0 ] ) ;

   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [] ;
                   extensions = [] },
     [ 1; 0; 0; 39; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 0; 1; 0 ] ) ;

   ( ClientHello { client_version = `TLS_1_1 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [] ;
                   extensions = [] },
     [ 1; 0; 0; 39; 3; 2 ] @ a_l @ a_l @ [ 0; 0; 0; 1; 0 ] ) ;

   ( ClientHello { client_version = `TLS_1_0 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [] ;
                   extensions = [] },
     [ 1; 0; 0; 39; 3; 1 ] @ a_l @ a_l @ [ 0; 0; 0; 1; 0 ] ) ;

   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [Packet.TLS_NULL_WITH_NULL_NULL] ;
                   extensions = [] },
     [ 1; 0; 0; 41; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 2; 0; 0; 1; 0 ] ) ;

   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = Packet.([TLS_NULL_WITH_NULL_NULL ; TLS_RSA_WITH_NULL_MD5 ; TLS_RSA_WITH_NULL_SHA ; TLS_RSA_WITH_3DES_EDE_CBC_SHA]);
                   extensions = [] },
     [ 1; 0; 0; 47; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 8; 0; 0; 0; 1; 0; 2; 0; 0x0a; 1; 0 ] ) ;


   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = Packet.([TLS_NULL_WITH_NULL_NULL ; TLS_RSA_WITH_NULL_MD5 ; TLS_RSA_WITH_NULL_SHA ; TLS_RSA_WITH_3DES_EDE_CBC_SHA]);
                   extensions = [
                            `SignatureAlgorithms
                              [`RSA_PKCS1_SHA512 ;
                               `RSA_PKCS1_SHA384 ;
                               `RSA_PKCS1_SHA256 ;
                               `RSA_PKCS1_SHA224 ;
                               `RSA_PKCS1_SHA1 ] ] },
     [ 1; 0; 0; 65; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 8; 0; 0; 0; 1; 0; 2; 0; 0x0a; 1; 0 ; 0; 0x10 ;

              0x00; 0x0d; 0x00; 0x0c; (* signature algorithms *)
              0x00; 0x0a; 0x06; 0x01; 0x05; 0x01; 0x04; 0x01; 0x03; 0x01; 0x02; 0x01 ] ) ;


   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [Packet.TLS_NULL_WITH_NULL_NULL] ;
                   extensions = [`ALPN ["h2"; "http/1.1"]] },
     [ 1; 0; 0; 61; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 2; 0; 0; 1; 0; 0; 18; 0; 16; 0; 14; 0; 12; 2; 104; 50; 8; 104; 116; 116; 112; 47; 49; 46; 49 ] ) ;


   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [Packet.TLS_NULL_WITH_NULL_NULL] ;
                   extensions = [`Hostname "foo"] },
     [ 1; 0; 0; 55; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 2; 0; 0; 1; 0; 0; 12; 0; 0; 0; 8; 0; 6; 0; 0; 3; 102; 111; 111 ] ) ;

   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [Packet.TLS_NULL_WITH_NULL_NULL] ;
                   extensions = [`Hostname "foofoofoofoofoofoofoofoofoofoo"] },
     [ 1; 0; 0; 82; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 2; 0; 0; 1; 0; 0; 39; 0; 0; 0; 35; 0; 33; 0; 0; 30; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111 ] ) ;

   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [Packet.TLS_NULL_WITH_NULL_NULL] ;
                   extensions = [`Hostname "foofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoo"] },
     [ 1; 0; 0; 232; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 2; 0; 0; 1; 0; 0; 189; 0; 0; 0; 185; 0; 183; 0; 0; 180; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111; 102; 111; 111 ] ) ;

   (* this one is the smallest which needs extra padding
     (due to its size being > 256 and < 511) *)
   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [Packet.TLS_NULL_WITH_NULL_NULL] ;
                   extensions = [`Hostname "foofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofo"] },
     [ 1; 0; 1; 0xFC; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 2; 0; 0; 1; 0; 1; 0xD1; 0; 0; 0; 0xD0; 0; 0xCE; 0; 0; 0xCB; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111; 0; 21; 0; 0xF9; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0  ] ) ;

   (* this one is the biggest which needs no extra padding *)
   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [Packet.TLS_NULL_WITH_NULL_NULL] ;
                   extensions = [`Hostname "foofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoof"] },
     [ 1; 0; 0; 251; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 2; 0; 0; 1; 0; 0; 208; 0; 0; 0; 204; 0; 202; 0; 0; 199; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102;111;111; 102 ] ) ;

   (* this one is the biggest which needs no extra padding, and no exts *)
   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;
Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;
Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;
Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL ] ;
                   extensions = [] },
     [ 1; 0; 0; 251; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 212; 0; 0;
0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;
  1; 0 ] ) ;

   (* add one more, and we get into padding no exts *)
   ( ClientHello { client_version = `TLS_1_2 ;
                   client_random = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuites = [Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL; Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;
Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;
Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;
Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL;Packet.TLS_NULL_WITH_NULL_NULL ] ;
                   extensions = [] },
     [ 1; 0; 1; 0xFC; 3; 3 ] @ a_l @ a_l @ [ 0; 0; 214; 0; 0; 0; 0;
0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;
  1; 0;
 0;0xFD;0;0x15;0;0xF9;
0;0;0;0;0;0;0;0;0;
0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0; 0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
 ] ) ;

   ( ServerHello { server_version = `TLS_1_2 ;
                   server_random  = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuite = `RSA_WITH_AES_128_CCM ;
                   extensions = []
                 } ,
     [2; 0; 0; 38; 3; 3] @ a_l @ a_l @ [(* session id *) 0; (* cipher *) 0xc0; 0x9c; (* comp *) 0; (* exts *)] ) ;

   ( ServerHello { server_version = `TLS_1_1 ;
                   server_random  = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuite = `RSA_WITH_AES_128_CCM ;
                   extensions = []
                 } ,
     [2; 0; 0; 38; 3; 2] @ a_l @ a_l @ [(* session id *) 0; (* cipher *) 0xc0; 0x9c; (* comp *) 0; (* exts *)] ) ;

   ( ServerHello { server_version = `TLS_1_0 ;
                   server_random  = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuite = `RSA_WITH_AES_128_CCM ;
                   extensions = []
                 } ,
     [2; 0; 0; 38; 3; 1] @ a_l @ a_l @ [(* session id *) 0; (* cipher *) 0xc0; 0x9c; (* comp *) 0; (* exts *)] ) ;


   ( ServerHello { server_version = `TLS_1_0 ;
                   server_random  = a_cs <+> a_cs ;
                   sessionid = Some a_cs ;
                   ciphersuite = `RSA_WITH_AES_128_CCM ;
                   extensions = []
                 } ,
     [2; 0; 0; 54; 3; 1] @ a_l @ a_l @ (* session id *) [ 16 ] @ a_l @ [(* cipher *) 0xc0; 0x9c; (* comp *) 0; (* exts *)] ) ;

   ( ServerHello { server_version = `TLS_1_2 ;
                   server_random  = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuite = `RSA_WITH_AES_128_CCM ;
                   extensions = [`Hostname]
                 } ,
     [2; 0; 0; 44; 3; 3] @ a_l @ a_l @ [(* session id *) 0; (* cipher *) 0xc0; 0x9c; (* comp *) 0; (* exts *) 0; 4; 0; 0; 0; 0] ) ;


   ( ServerHello { server_version = `TLS_1_2 ;
                   server_random  = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuite = `RSA_WITH_AES_128_CCM ;
                   extensions = [`SecureRenegotiation (Cstruct.create 0)]
                 } ,
     [2; 0; 0; 45; 3; 3] @ a_l @ a_l @ [(* session id *) 0; (* cipher *) 0xc0; 0x9c; (* comp *) 0; (* exts *) 0; 5; 0xFF; 1; 0; 1; 0] ) ;

   ( ServerHello { server_version = `TLS_1_2 ;
                   server_random  = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuite = `RSA_WITH_AES_128_CCM ;
                   extensions = [`Hostname ; `SecureRenegotiation (Cstruct.create 0)]
                 } ,
     [2; 0; 0; 49; 3; 3] @ a_l @ a_l @ [(* session id *) 0; (* cipher *) 0xc0; 0x9c; (* comp *) 0; (* exts *) 0; 9; 0; 0; 0; 0; 0xFF; 1; 0; 1; 0] ) ;

   ( ServerHello { server_version = `TLS_1_2 ;
                   server_random  = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuite = `RSA_WITH_AES_128_CCM ;
                   extensions = [`SecureRenegotiation (Cstruct.create 0); `Hostname ]
                 } ,
     [2; 0; 0; 49; 3; 3] @ a_l @ a_l @ [(* session id *) 0; (* cipher *) 0xc0; 0x9c; (* comp *) 0; (* exts *) 0; 9; 0xFF; 1; 0; 1; 0; 0; 0; 0; 0] ) ;

   ( ServerHello { server_version = `TLS_1_2 ;
                   server_random  = a_cs <+> a_cs ;
                   sessionid = None ;
                   ciphersuite = `RSA_WITH_AES_128_CCM ;
                   extensions = [`ALPN "h2"]
                 } ,
     [2; 0; 0; 49; 3; 3] @ a_l @ a_l @ [(* session id *) 0; (* cipher *) 0xc0; 0x9c; (* comp *) 0; (* exts *) 0; 9; 0; 16; 0; 5; 0; 3; 2; 104; 50] ) ;

(*  | CertificateRequest of certificate_request *)
  ])

let handshake_tests =
  List.mapi
    (fun i f -> "Assemble handshake " ^ string_of_int i >:: handshake_assembler f)
    handshake_assembler_tests

let writer_tests =
  version_tests @
  hdr_tests @
  alert_tests @
  ["CCS " >:: ccs_test] @
  dh_tests @
  ds_tests @
  ds_1_2_tests @
  handshake_tests
