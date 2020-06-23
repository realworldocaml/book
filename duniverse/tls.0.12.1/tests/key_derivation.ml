(* key derivation example trace taken from draft-ietf-tls-tls13-vectors-07 *)
let cs =
  let module M = struct
    type t = Cstruct.t
    let pp = Cstruct.hexdump_pp
    let equal = Cstruct.equal
  end in (module M : Alcotest.TESTABLE with type t = M.t)

let secret0 = Cstruct.of_hex {|
33 ad 0a 1c 60 7e c0 3b  09 e6 cd 98 93 68 0c e2
10 ad f3 00 aa 1f 26 60  e1 b2 2e 10 f1 70 f9 2a
|}

let cipher = `AES_128_GCM_SHA256
let hash = Tls.Ciphersuite.hash13 cipher

let my_secret = ref None

let extract_secret_early () =
  (* draft-ietf-tls-tls13-vectors-07 Sec 3*)
  let salt = Cstruct.empty
  and ikm = Cstruct.create 32
  in
  Alcotest.check cs __LOC__ secret0 (Hkdf.extract ~hash ~salt ikm) ;
  let t = Tls.Handshake_crypto13.(derive (empty cipher) ikm) in
  my_secret := Some t ;
  Alcotest.check cs __LOC__ secret0 t.secret

let expand0 = Cstruct.of_hex {|
6f 26 15 a1 08 c7 02 c5  67 8f 54 fc 9d ba b6 97
16 c0 76 18 9c 48 25 0c  eb ea c3 57 6c 36 11 ba
|}

let derive_hs_secret () =
  let hash_val = Mirage_crypto.Hash.digest hash Cstruct.empty in
  Alcotest.check cs __LOC__ expand0
    (Tls.Handshake_crypto13.derive_secret_no_hash hash secret0 ~ctx:hash_val "derived")

let hs_secret = Cstruct.of_hex {|
1d c8 26 e9 36 06 aa 6f  dc 0a ad c1 2f 74 1b 01
04 6a a6 b9 9f 69 1e d2  21 a9 f0 ca 04 3f be ac
|}

(* TODO: ikm should be computed (ECDHE) from the key share in client hello (see
   below), and the private key written in the RFC. *)
let ikm = Cstruct.of_hex {|
8b d4 05 4f b5 5b 9d 63  fd fb ac f9 f0 4b 9f 0d
35 e6 d6 3f 53 75 63 ef  d4 62 72 90 0f 89 49 2d
|}

let extract_handshake () =
  Alcotest.check cs __LOC__ hs_secret (Hkdf.extract ~hash ~salt:expand0 ikm) ;
  match !my_secret with
  | None -> Alcotest.fail "expected some secret"
  | Some t ->
    let t' = Tls.Handshake_crypto13.derive t ikm in
    my_secret := Some t' ;
    Alcotest.check cs __LOC__ hs_secret t'.secret

let ch = Cstruct.of_hex {|
01 00 00 c0 03 03 cb 34  ec b1 e7 81 63 ba 1c 38
c6 da cb 19 6a 6d ff a2  1a 8d 99 12 ec 18 a2 ef
62 83 02 4d ec e7 00 00  06 13 01 13 03 13 02 01
00 00 91 00 00 00 0b 00  09 00 00 06 73 65 72 76
65 72 ff 01 00 01 00 00  0a 00 14 00 12 00 1d 00
17 00 18 00 19 01 00 01  01 01 02 01 03 01 04 00
23 00 00 00 33 00 26 00  24 00 1d 00 20 99 38 1d
e5 60 e4 bd 43 d2 3d 8e  43 5a 7d ba fe b3 c0 6e
51 c1 3c ae 4d 54 13 69  1e 52 9a af 2c 00 2b 00
03 02 03 04 00 0d 00 20  00 1e 04 03 05 03 06 03
02 03 08 04 08 05 08 06  04 01 05 01 06 01 02 01
04 02 05 02 06 02 02 02  00 2d 00 02 01 01 00 1c
00 02 40 01
|}

let sh = Cstruct.of_hex {|
02 00 00 56 03 03 a6 af  06 a4 12 18 60 dc 5e 6e
60 24 9c d3 4c 95 93 0c  8a c5 cb 14 34 da c1 55
77 2e d3 e2 69 28 00 13  01 00 00 2e 00 33 00 24
00 1d 00 20 c9 82 88 76  11 20 95 fe 66 76 2b db
f7 c6 72 e1 56 d6 cc 25  3b 83 3d f1 dd 69 b1 b0
4e 75 1f 0f 00 2b 00 02  03 04
|}

let c_hs_traffic_secret = Cstruct.of_hex {|
b3 ed db 12 6e 06 7f 35  a7 80 b3 ab f4 5e 2d 8f
3b 1a 95 07 38 f5 2e 96  00 74 6a 0e 27 a5 5a 21
|}

let read_handshake_key = Cstruct.of_hex {|
db fa a6 93 d1 76 2c 5b  66 6a f5 d9 50 25 8d 01
|}

let read_handshake_iv = Cstruct.of_hex {|
5b d3 c7 1b 83 6e 0b 76  bb 73 26 5f
|}

let derive_c_hs_traffic () =
  let log = Cstruct.append ch sh in
  let hash_val = Mirage_crypto.Hash.digest hash log in
  Alcotest.check cs __LOC__ c_hs_traffic_secret
    (Tls.Handshake_crypto13.derive_secret_no_hash hash hs_secret ~ctx:hash_val "c hs traffic") ;
  match !my_secret with
  | None -> Alcotest.fail "expected my secret"
  | Some t ->
    let c_hs_traffic = Tls.Handshake_crypto13.derive_secret t "c hs traffic" log in
    Alcotest.check cs __LOC__ c_hs_traffic_secret c_hs_traffic ;
    let key, iv = Tls.Handshake_crypto13.traffic_key cipher c_hs_traffic in
    Alcotest.check cs __LOC__ read_handshake_key key ;
    Alcotest.check cs __LOC__ read_handshake_iv iv

let derive_read_handshake_keys () =
  Alcotest.check cs __LOC__ read_handshake_key
    (Tls.Handshake_crypto13.derive_secret_no_hash hash c_hs_traffic_secret ~length:16 "key") ;
  Alcotest.check cs __LOC__ read_handshake_iv
    (Tls.Handshake_crypto13.derive_secret_no_hash hash c_hs_traffic_secret ~length:12 "iv")

let s_hs_traffic_secret = Cstruct.of_hex {|
b6 7b 7d 69 0c c1 6c 4e  75 e5 42 13 cb 2d 37 b4
e9 c9 12 bc de d9 10 5d  42 be fd 59 d3 91 ad 38
|}

let write_handshake_key = Cstruct.of_hex {|
3f ce 51 60 09 c2 17 27  d0 f2 e4 e8 6e e4 03 bc
|}

let write_handshake_iv = Cstruct.of_hex {|
5d 31 3e b2 67 12 76 ee  13 00 0b 30
|}

let derive_s_hs_traffic () =
  let log = Cstruct.append ch sh in
  let hash_val = Mirage_crypto.Hash.digest hash log in
  Alcotest.check cs __LOC__ s_hs_traffic_secret
    (Tls.Handshake_crypto13.derive_secret_no_hash hash hs_secret ~ctx:hash_val "s hs traffic") ;
  match !my_secret with
  | None -> Alcotest.fail "expected my secret"
  | Some t ->
    let s_hs_traffic = Tls.Handshake_crypto13.derive_secret t "s hs traffic" log in
    Alcotest.check cs __LOC__ s_hs_traffic_secret s_hs_traffic ;
    let key, iv = Tls.Handshake_crypto13.traffic_key cipher s_hs_traffic in
    Alcotest.check cs __LOC__ write_handshake_key key ;
    Alcotest.check cs __LOC__ write_handshake_iv iv

let derive_write_handshake_keys () =
  Alcotest.check cs __LOC__ write_handshake_key
    (Tls.Handshake_crypto13.derive_secret_no_hash hash s_hs_traffic_secret ~length:16 "key") ;
  Alcotest.check cs __LOC__ write_handshake_iv
    (Tls.Handshake_crypto13.derive_secret_no_hash hash s_hs_traffic_secret ~length:12"iv")

let finished_expanded = Cstruct.of_hex {|
00 8d 3b 66 f8 16 ea 55  9f 96 b5 37 e8 85 c3 1f
c0 68 bf 49 2c 65 2f 01  f2 88 a1 d8 cd c1 9f c8
|}

let finished_key = Cstruct.of_hex {|
9b 9b 14 1d 90 63 37 fb  d2 cb dc e7 1d f4 de da
4a b4 2c 30 95 72 cb 7f  ff ee 54 54 b7 8f 07 18
|}

let enc_ext = Cstruct.of_hex {|
08 00 00 24 00 22 00 0a  00 14 00 12 00 1d 00 17
00 18 00 19 01 00 01 01  01 02 01 03 01 04 00 1c
00 02 40 01 00 00 00 00
|}

let cert = Cstruct.of_hex {|
0b 00 01 b9 00 00 01 b5  00 01 b0 30 82 01 ac 30
82 01 15 a0 03 02 01 02  02 01 02 30 0d 06 09 2a
86 48 86 f7 0d 01 01 0b  05 00 30 0e 31 0c 30 0a
06 03 55 04 03 13 03 72  73 61 30 1e 17 0d 31 36
30 37 33 30 30 31 32 33  35 39 5a 17 0d 32 36 30
37 33 30 30 31 32 33 35  39 5a 30 0e 31 0c 30 0a
06 03 55 04 03 13 03 72  73 61 30 81 9f 30 0d 06
09 2a 86 48 86 f7 0d 01  01 01 05 00 03 81 8d 00
30 81 89 02 81 81 00 b4  bb 49 8f 82 79 30 3d 98
08 36 39 9b 36 c6 98 8c  0c 68 de 55 e1 bd b8 26
d3 90 1a 24 61 ea fd 2d  e4 9a 91 d0 15 ab bc 9a
95 13 7a ce 6c 1a f1 9e  aa 6a f9 8c 7c ed 43 12
09 98 e1 87 a8 0e e0 cc  b0 52 4b 1b 01 8c 3e 0b
63 26 4d 44 9a 6d 38 e2  2a 5f da 43 08 46 74 80
30 53 0e f0 46 1c 8c a9  d9 ef bf ae 8e a6 d1 d0
3e 2b d1 93 ef f0 ab 9a  80 02 c4 74 28 a6 d3 5a
8d 88 d7 9f 7f 1e 3f 02  03 01 00 01 a3 1a 30 18
30 09 06 03 55 1d 13 04  02 30 00 30 0b 06 03 55
1d 0f 04 04 03 02 05 a0  30 0d 06 09 2a 86 48 86
f7 0d 01 01 0b 05 00 03  81 81 00 85 aa d2 a0 e5
b9 27 6b 90 8c 65 f7 3a  72 67 17 06 18 a5 4c 5f
8a 7b 33 7d 2d f7 a5 94  36 54 17 f2 ea e8 f8 a5
8c 8f 81 72 f9 31 9c f3  6b 7f d6 c5 5b 80 f2 1a
03 01 51 56 72 60 96 fd  33 5e 5e 67 f2 db f1 02
70 2e 60 8c ca e6 be c1  fc 63 a4 2a 99 be 5c 3e
b7 10 7c 3c 54 e9 b9 eb  2b d5 20 3b 1c 3b 84 e0
a8 b2 f7 59 40 9b a3 ea  c9 d9 1d 40 2d cc 0c c8
f8 96 12 29 ac 91 87 b4  2b 4d e1 00 00
|}

let cert_verify = Cstruct.of_hex {|
0f 00 00 84 08 04 00 80  5a 74 7c 5d 88 fa 9b d2
e5 5a b0 85 a6 10 15 b7  21 1f 82 4c d4 84 14 5a
b3 ff 52 f1 fd a8 47 7b  0b 7a bc 90 db 78 e2 d3
3a 5c 14 1a 07 86 53 fa  6b ef 78 0c 5e a2 48 ee
aa a7 85 c4 f3 94 ca b6  d3 0b be 8d 48 59 ee 51
1f 60 29 57 b1 54 11 ac  02 76 71 45 9e 46 44 5c
9e a5 8c 18 1e 81 8e 95  b8 c3 fb 0b f3 27 84 09
d3 be 15 2a 3d a5 04 3e  06 3d da 65 cd f5 ae a2
0d 53 df ac d4 2f 74 f3
|}

let derive_finished () =
  let log = Cstruct.concat [ ch ; sh ; enc_ext ; cert ; cert_verify ] in
  Alcotest.check cs __LOC__ finished_expanded
    (Tls.Handshake_crypto13.derive_secret_no_hash hash s_hs_traffic_secret "finished") ;
  let hash_val = Mirage_crypto.Hash.digest hash log in
  Alcotest.check cs __LOC__ finished_key
    (Mirage_crypto.Hash.mac hash ~key:finished_expanded hash_val) ;
  match !my_secret with
  | None -> Alcotest.fail "expected some secret"
  | Some t ->
    let s_hs_traffic = Tls.Handshake_crypto13.derive_secret t "s hs traffic" (Cstruct.append ch sh) in
    Alcotest.check cs __LOC__ s_hs_traffic_secret s_hs_traffic ;
    Alcotest.check cs __LOC__ finished_key
      (Tls.Handshake_crypto13.finished t.hash s_hs_traffic log)

let finished = Cstruct.of_hex {|
14 00 00 20 9b 9b 14 1d  90 63 37 fb d2 cb dc e7
1d f4 de da 4a b4 2c 30  95 72 cb 7f ff ee 54 54
b7 8f 07 18
|}

let master = Cstruct.of_hex {|
43 de 77 e0 c7 77 13 85  9a 94 4d b9 db 25 90 b5
31 90 a6 5b 3e e2 e4 f1  2d d7 a0 bb 7c e2 54 b4
|}

let derive_master () =
  let hash_val = Mirage_crypto.Hash.digest hash Cstruct.empty in
  Alcotest.check cs __LOC__ master
    (Tls.Handshake_crypto13.derive_secret_no_hash hash hs_secret ~ctx:hash_val "derived")

let master_secret = Cstruct.of_hex {|
18 df 06 84 3d 13 a0 8b  f2 a4 49 84 4c 5f 8a 47
80 01 bc 4d 4c 62 79 84  d5 a4 1d a8 d0 40 29 19
|}

let extract_master () =
  Alcotest.check cs __LOC__ master_secret (Hkdf.extract ~hash ~salt:master (Cstruct.create 32)) ;
  match !my_secret with
  | None -> Alcotest.fail "expected my secret"
  | Some t ->
    let t' = Tls.Handshake_crypto13.derive t (Cstruct.create 32) in
    my_secret := Some t' ;
    Alcotest.check cs __LOC__ master_secret t'.secret

let c_ap_traffic = Cstruct.of_hex {|
9e 40 64 6c e7 9a 7f 9d  c0 5a f8 88 9b ce 65 52
87 5a fa 0b 06 df 00 87  f7 92 eb b7 c1 75 04 a5
|}

let s_ap_traffic = Cstruct.of_hex {|
a1 1a f9 f0 55 31 f8 56  ad 47 11 6b 45 a9 50 32
82 04 b4 f4 4b fb 6b 3a  4b 4f 1f 3f cb 63 16 43
|}

let exp_master = Cstruct.of_hex {|
fe 22 f8 81 17 6e da 18  eb 8f 44 52 9e 67 92 c5
0c 9a 3f 89 45 2f 68 d8  ae 31 1b 43 09 d3 cf 50
|}

let app_write_key = Cstruct.of_hex {|
9f 02 28 3b 6c 9c 07 ef  c2 6b b9 f2 ac 92 e3 56
|}

let app_write_iv = Cstruct.of_hex {|
cf 78 2b 88 dd 83 54 9a  ad f1 e9 84
|}

let app_read_key = Cstruct.of_hex {|
17 42 2d da 59 6e d5 d9  ac d8 90 e3 c6 3f 50 51
|}

let app_read_iv = Cstruct.of_hex {|
5b 78 92 3d ee 08 57 90  33 e5 23 d9
|}

let derive_traffic_keys () =
  let log = Cstruct.concat [ ch ; sh ; enc_ext ; cert ; cert_verify ; finished ] in
  let hash_val = Mirage_crypto.Hash.digest hash log in
  Alcotest.check cs __LOC__ c_ap_traffic
    (Tls.Handshake_crypto13.derive_secret_no_hash hash master_secret ~ctx:hash_val "c ap traffic") ;
  Alcotest.check cs __LOC__ s_ap_traffic
    (Tls.Handshake_crypto13.derive_secret_no_hash hash master_secret ~ctx:hash_val "s ap traffic") ;
  Alcotest.check cs __LOC__ exp_master
    (Tls.Handshake_crypto13.derive_secret_no_hash hash master_secret ~ctx:hash_val "exp master") ;
  match !my_secret with
  | None -> Alcotest.fail "expected some secret"
  | Some t ->
    let c_ap_traffic' = Tls.Handshake_crypto13.derive_secret t "c ap traffic" log in
    Alcotest.check cs __LOC__ c_ap_traffic c_ap_traffic' ;
    let key, iv = Tls.Handshake_crypto13.traffic_key cipher c_ap_traffic' in
    Alcotest.check cs __LOC__ app_read_key key ;
    Alcotest.check cs __LOC__ app_read_iv iv ;
    let s_ap_traffic' = Tls.Handshake_crypto13.derive_secret t "s ap traffic" log in
    Alcotest.check cs __LOC__ s_ap_traffic s_ap_traffic' ;
    let key, iv = Tls.Handshake_crypto13.traffic_key cipher s_ap_traffic' in
    Alcotest.check cs __LOC__ app_write_key key ;
    Alcotest.check cs __LOC__ app_write_iv iv

let appdata_write () =
  Alcotest.check cs __LOC__ app_write_key
    (Tls.Handshake_crypto13.derive_secret_no_hash hash s_ap_traffic ~length:16 "key") ;
  Alcotest.check cs __LOC__ app_write_iv
    (Tls.Handshake_crypto13.derive_secret_no_hash hash s_ap_traffic ~length:12 "iv")

let appdata_read () =
  Alcotest.check cs __LOC__ app_read_key
    (Tls.Handshake_crypto13.derive_secret_no_hash hash c_ap_traffic ~length:16 "key") ;
  Alcotest.check cs __LOC__ app_read_iv
    (Tls.Handshake_crypto13.derive_secret_no_hash hash c_ap_traffic ~length:12 "iv")

let server_payload = Cstruct.of_hex {|
08 00 00 24 00 22 00 0a  00 14 00 12 00 1d 00 17
00 18 00 19 01 00 01 01  01 02 01 03 01 04 00 1c
00 02 40 01 00 00 00 00  0b 00 01 b9 00 00 01 b5
00 01 b0 30 82 01 ac 30  82 01 15 a0 03 02 01 02
02 01 02 30 0d 06 09 2a  86 48 86 f7 0d 01 01 0b
05 00 30 0e 31 0c 30 0a  06 03 55 04 03 13 03 72
73 61 30 1e 17 0d 31 36  30 37 33 30 30 31 32 33
35 39 5a 17 0d 32 36 30  37 33 30 30 31 32 33 35
39 5a 30 0e 31 0c 30 0a  06 03 55 04 03 13 03 72
73 61 30 81 9f 30 0d 06  09 2a 86 48 86 f7 0d 01
01 01 05 00 03 81 8d 00  30 81 89 02 81 81 00 b4
bb 49 8f 82 79 30 3d 98  08 36 39 9b 36 c6 98 8c
0c 68 de 55 e1 bd b8 26  d3 90 1a 24 61 ea fd 2d
e4 9a 91 d0 15 ab bc 9a  95 13 7a ce 6c 1a f1 9e
aa 6a f9 8c 7c ed 43 12  09 98 e1 87 a8 0e e0 cc
b0 52 4b 1b 01 8c 3e 0b  63 26 4d 44 9a 6d 38 e2
2a 5f da 43 08 46 74 80  30 53 0e f0 46 1c 8c a9
d9 ef bf ae 8e a6 d1 d0  3e 2b d1 93 ef f0 ab 9a
80 02 c4 74 28 a6 d3 5a  8d 88 d7 9f 7f 1e 3f 02
03 01 00 01 a3 1a 30 18  30 09 06 03 55 1d 13 04
02 30 00 30 0b 06 03 55  1d 0f 04 04 03 02 05 a0
30 0d 06 09 2a 86 48 86  f7 0d 01 01 0b 05 00 03
81 81 00 85 aa d2 a0 e5  b9 27 6b 90 8c 65 f7 3a
72 67 17 06 18 a5 4c 5f  8a 7b 33 7d 2d f7 a5 94
36 54 17 f2 ea e8 f8 a5  8c 8f 81 72 f9 31 9c f3
6b 7f d6 c5 5b 80 f2 1a  03 01 51 56 72 60 96 fd
33 5e 5e 67 f2 db f1 02  70 2e 60 8c ca e6 be c1
fc 63 a4 2a 99 be 5c 3e  b7 10 7c 3c 54 e9 b9 eb
2b d5 20 3b 1c 3b 84 e0  a8 b2 f7 59 40 9b a3 ea
c9 d9 1d 40 2d cc 0c c8  f8 96 12 29 ac 91 87 b4
2b 4d e1 00 00 0f 00 00  84 08 04 00 80 5a 74 7c
5d 88 fa 9b d2 e5 5a b0  85 a6 10 15 b7 21 1f 82
4c d4 84 14 5a b3 ff 52  f1 fd a8 47 7b 0b 7a bc
90 db 78 e2 d3 3a 5c 14  1a 07 86 53 fa 6b ef 78
0c 5e a2 48 ee aa a7 85  c4 f3 94 ca b6 d3 0b be
8d 48 59 ee 51 1f 60 29  57 b1 54 11 ac 02 76 71
45 9e 46 44 5c 9e a5 8c  18 1e 81 8e 95 b8 c3 fb
0b f3 27 84 09 d3 be 15  2a 3d a5 04 3e 06 3d da
65 cd f5 ae a2 0d 53 df  ac d4 2f 74 f3 14 00 00
20 9b 9b 14 1d 90 63 37  fb d2 cb dc e7 1d f4 de
da 4a b4 2c 30 95 72 cb  7f ff ee 54 54 b7 8f 07
18
|}

let payload_is_good () =
  Alcotest.check cs __LOC__ server_payload
    (Cstruct.concat [ enc_ext ; cert ; cert_verify ; finished ])

let server_payload_processed = Cstruct.of_hex {|
17 03 03 02 a2 d1 ff 33   4a 56 f5 bf f6 59 4a 07
cc 87 b5 80 23 3f 50 0f  45 e4 89 e7 f3 3a f3 5e
df 78 69 fc f4 0a a4 0a  a2 b8 ea 73 f8 48 a7 ca
07 61 2e f9 f9 45 cb 96  0b 40 68 90 51 23 ea 78
b1 11 b4 29 ba 91 91 cd  05 d2 a3 89 28 0f 52 61
34 aa dc 7f c7 8c 4b 72  9d f8 28 b5 ec f7 b1 3b
d9 ae fb 0e 57 f2 71 58  5b 8e a9 bb 35 5c 7c 79
02 07 16 cf b9 b1 18 3e  f3 ab 20 e3 7d 57 a6 b9
d7 47 76 09 ae e6 e1 22  a4 cf 51 42 73 25 25 0c
7d 0e 50 92 89 44 4c 9b  3a 64 8f 1d 71 03 5d 2e
d6 5b 0e 3c dd 0c ba e8  bf 2d 0b 22 78 12 cb b3
60 98 72 55 cc 74 41 10  c4 53 ba a4 fc d6 10 92
8d 80 98 10 e4 b7 ed 1a  8f d9 91 f0 6a a6 24 82
04 79 7e 36 a6 a7 3b 70  a2 55 9c 09 ea d6 86 94
5b a2 46 ab 66 e5 ed d8  04 4b 4c 6d e3 fc f2 a8
94 41 ac 66 27 2f d8 fb  33 0e f8 19 05 79 b3 68
45 96 c9 60 bd 59 6e ea  52 0a 56 a8 d6 50 f5 63
aa d2 74 09 96 0d ca 63  d3 e6 88 61 1e a5 e2 2f
44 15 cf 95 38 d5 1a 20  0c 27 03 42 72 96 8a 26
4e d6 54 0c 84 83 8d 89  f7 2c 24 46 1a ad 6d 26
f5 9e ca ba 9a cb bb 31  7b 66 d9 02 f4 f2 92 a3
6a c1 b6 39 c6 37 ce 34  31 17 b6 59 62 22 45 31
7b 49 ee da 0c 62 58 f1  00 d7 d9 61 ff b1 38 64
7e 92 ea 33 0f ae ea 6d  fa 31 c7 a8 4d c3 bd 7e
1b 7a 6c 71 78 af 36 87  90 18 e3 f2 52 10 7f 24
3d 24 3d c7 33 9d 56 84  c8 b0 37 8b f3 02 44 da
8c 87 c8 43 f5 e5 6e b4  c5 e8 28 0a 2b 48 05 2c
f9 3b 16 49 9a 66 db 7c  ca 71 e4 59 94 26 f7 d4
61 e6 6f 99 88 2b d8 9f  c5 08 00 be cc a6 2d 6c
74 11 6d bd 29 72 fd a1  fa 80 f8 5d f8 81 ed be
5a 37 66 89 36 b3 35 58  3b 59 91 86 dc 5c 69 18
a3 96 fa 48 a1 81 d6 b6  fa 4f 9d 62 d5 13 af bb
99 2f 2b 99 2f 67 f8 af  e6 7f 76 91 3f a3 88 cb
56 30 c8 ca 01 e0 c6 5d  11 c6 6a 1e 2a c4 c8 59
77 b7 c7 a6 99 9b bf 10  dc 35 ae 69 f5 51 56 14
63 6c 0b 9b 68 c1 9e d2  e3 1c 0b 3b 66 76 30 38
eb ba 42 f3 b3 8e dc 03  99 f3 a9 f2 3f aa 63 97
8c 31 7f c9 fa 66 a7 3f  60 f0 50 4d e9 3b 5b 84
5e 27 55 92 c1 23 35 ee  34 0b bc 4f dd d5 02 78
40 16 e4 b3 be 7e f0 4d  da 49 f4 b4 40 a3 0c b5
d2 af 93 98 28 fd 4a e3  79 4e 44 f9 4d f5 a6 31
ed e4 2c 17 19 bf da bf  02 53 fe 51 75 be 89 8e
75 0e dc 53 37 0d 2b
|}

let processed_payload () =
  let buf =
    let t = Cstruct.create 1 in
    Cstruct.set_uint8 t 0 (Tls.Packet.content_type_to_int Tls.Packet.HANDSHAKE) ;
    Cstruct.append server_payload t
  in
  let nonce = Tls.Crypto.aead_nonce write_handshake_iv 0L in
  let key = Mirage_crypto.Cipher_block.AES.GCM.of_secret write_handshake_key in
  let adata = Tls.Writer.assemble_hdr `TLS_1_2 (Tls.Packet.APPLICATION_DATA, Cstruct.empty) in
  Cstruct.BE.set_uint16 adata 3 (17 + Cstruct.len server_payload) ;
  let res = Mirage_crypto.Cipher_block.AES.GCM.encrypt ~key ~adata ~iv:nonce buf in
  let buf' = Cstruct.append res.message res.tag in
  let data = Tls.Writer.assemble_hdr `TLS_1_2 (Tls.Packet.APPLICATION_DATA, buf') in
  Alcotest.check cs __LOC__ server_payload_processed data

let c_finished = Cstruct.of_hex {|
14 00 00 20 a8 ec 43 6d  67 76 34 ae 52 5a c1 fc
eb e1 1a 03 9e c1 76 94  fa c6 e9 85 27 b6 42 f2
ed d5 ce 61
|}

let res_master = Cstruct.of_hex {|
7d f2 35 f2 03 1d 2a 05  12 87 d0 2b 02 41 b0 bf
da f8 6c c8 56 23 1f 2d  5a ba 46 c4 34 ec 19 6c
|}

let resumption () =
  let log = Cstruct.concat [ ch ; sh ; enc_ext ; cert ; cert_verify ; finished ; c_finished ] in
  let hash_val = Mirage_crypto.Hash.digest hash log in
  Alcotest.check cs __LOC__ res_master
    (Tls.Handshake_crypto13.derive_secret_no_hash hash master_secret ~ctx:hash_val "res master") ;
  match !my_secret with
  | None -> Alcotest.fail "expected some secret"
  | Some s -> Alcotest.check cs __LOC__ res_master (Tls.Handshake_crypto13.resumption s log)

let private_key =
  let _modulus = Cstruct.of_hex {|
b4 bb 49 8f 82 79 30 3d  98 08 36 39 9b 36 c6 98
8c 0c 68 de 55 e1 bd b8  26 d3 90 1a 24 61 ea fd
2d e4 9a 91 d0 15 ab bc  9a 95 13 7a ce 6c 1a f1
9e aa 6a f9 8c 7c ed 43  12 09 98 e1 87 a8 0e e0
cc b0 52 4b 1b 01 8c 3e  0b 63 26 4d 44 9a 6d 38
e2 2a 5f da 43 08 46 74  80 30 53 0e f0 46 1c 8c
a9 d9 ef bf ae 8e a6 d1  d0 3e 2b d1 93 ef f0 ab
9a 80 02 c4 74 28 a6 d3  5a 8d 88 d7 9f 7f 1e 3f
|}
  and public_exponent = Cstruct.of_hex "01 00 01"
  and _private_exponent = Cstruct.of_hex {|
04 de a7 05 d4 3a 6e a7  20 9d d8 07 21 11 a8 3c
81 e3 22 a5 92 78 b3 34  80 64 1e af 7c 0a 69 85
b8 e3 1c 44 f6 de 62 e1  b4 c2 30 9f 61 26 e7 7b
7c 41 e9 23 31 4b bf a3  88 13 05 dc 12 17 f1 6c
81 9c e5 38 e9 22 f3 69  82 8d 0e 57 19 5d 8c 84
88 46 02 07 b2 fa a7 26  bc f7 08 bb d7 db 7f 67
9f 89 34 92 fc 2a 62 2e  08 97 0a ac 44 1c e4 e0
c3 08 8d f2 5a e6 79 23  3d f8 a3 bd a2 ff 99 41
|}
  and prime1 = Cstruct.of_hex {|
e4 35 fb 7c c8 37 37 75 6d ac ea 96 ab 7f 59 a2
cc 10 69 db 7d eb 19 0e 17 e3 3a 53 2b 27 3f 30
a3 27 aa 0a aa bc 58 cd 67 46 6a f9 84 5f ad c6
75 fe 09 4a f9 2c 4b d1 f2 c1 bc 33 dd 2e 05 15
|}
  and prime2 = Cstruct.of_hex {|
ca bd 3b c0 e0 43 86 64 c8 d4 cc 9f 99 97 7a 94
d9 bb fe ad 8e 43 87 0a ba e3 f7 eb 8b 4e 0e ee
8a f1 d9 b4 71 9b a6 19 6c f2 cb ba ee eb f8 b3
49 0a fe 9e 9f fa 74 a8 8a a5 1f c6 45 62 93 03
|}
  and _exponent1 = Cstruct.of_hex {|
3f 57 34 5c 27 fe 1b 68 7e 6e 76 16 27 b7 8b 1b
82 64 33 dd 76 0f a0 be a6 a6 ac f3 94 90 aa 1b
47 cd a4 86 9d 68 f5 84 dd 5b 50 29 bd 32 09 3b
82 58 66 1f e7 15 02 5e 5d 70 a4 5a 08 d3 d3 19
|}
  and _exponent2 = Cstruct.of_hex {|
18 3d a0 13 63 bd 2f 28 85 ca cb dc 99 64 bf 47
64 f1 51 76 36 f8 64 01 28 6f 71 89 3c 52 cc fe
40 a6 c2 3d 0d 08 6b 47 c6 fb 10 d8 fd 10 41 e0
4d ef 7e 9a 40 ce 95 7c 41 77 94 e1 04 12 d1 39
|}
  and _coefficient = Cstruct.of_hex {|
83 9c a9 a0 85 e4 28 6b 2c 90 e4 66 99 7a 2c 68
1f 21 33 9a a3 47 78 14 e4 de c1 18 33 05 0e d5
0d d1 3c c0 38 04 8a 43 c5 9b 2a cc 41 68 89 c0
37 66 5f e5 af a6 05 96 9f 8c 01 df a5 ca 96 9d
|}
  in
  let e = Mirage_crypto_pk.Z_extra.of_cstruct_be public_exponent
  and p = Mirage_crypto_pk.Z_extra.of_cstruct_be prime1
  and q = Mirage_crypto_pk.Z_extra.of_cstruct_be prime2
  in
  match Mirage_crypto_pk.Rsa.priv_of_primes ~e ~p ~q with
  | Ok p -> p
  | Error (`Msg m) -> invalid_arg m

let log = Cstruct.concat [ ch ; sh ; enc_ext ; cert ]
and cert = match X509.Certificate.decode_der (Cstruct.sub cert 11 0x01b0) with
  | Ok c -> Some c
  | Error _ -> None

let self_signature () =
  match
    Tls.Handshake_common.signature `TLS_1_3
      ~context_string:"TLS 1.3, server CertificateVerify"
      (Mirage_crypto.Hash.digest hash log)
      None [ `RSA_PSS_RSAENC_SHA256 ] private_key
  with
  | Error _ -> Alcotest.fail "expected sth"
  | Ok data ->
    match Tls.Handshake_common.verify_digitally_signed `TLS_1_3
            ~context_string:"TLS 1.3, server CertificateVerify"
             [ `RSA_PSS_RSAENC_SHA256 ] data
             (Mirage_crypto.Hash.digest hash log) cert
    with
    | Ok () -> ()
    | Error e -> Alcotest.fail ("self-verification failed " ^ (Tls.Engine.string_of_failure e))

let wire_signature () =
  (* let buf = Writer.assemble_handshake (CertificateVerify data) in
     Alcotest.check cs __LOC__ cert_verify buf *)
  match Tls.Handshake_common.verify_digitally_signed `TLS_1_3
          ~context_string:"TLS 1.3, server CertificateVerify"
          [ `RSA_PSS_RSAENC_SHA256 ] (Cstruct.shift cert_verify 4)
          (Mirage_crypto.Hash.digest hash log) cert
  with
  | Ok () -> ()
  | Error e -> Alcotest.fail ("trace-verification failed " ^ (Tls.Engine.string_of_failure e))

let res_secret_00 = Cstruct.of_hex {|
4e cd 0e b6 ec 3b 4d 87  f5 d6 02 8f 92 2c a4 c5
85 1a 27 7f d4 13 11 c9  e6 2d 2c 94 92 e1 c4 f3
|}

let res_secret () =
  let nonce = Cstruct.create 2 in
  Alcotest.check cs __LOC__ res_secret_00
    (Tls.Handshake_crypto13.derive_secret_no_hash hash res_master ~ctx:nonce "resumption") ;
  Alcotest.check cs __LOC__ res_secret_00
    (Tls.Handshake_crypto13.res_secret hash res_master nonce)


let early_secret1 = Cstruct.of_hex {|
9b 21 88 e9 b2 fc 6d 64  d7 1d c3 29 90 0e 20 bb
41 91 50 00 f6 78 aa 83  9c bb 79 7c b7 d8 33 2c
|}

let early1 () =
  let salt = Cstruct.empty
  and ikm = res_secret_00
  in
  Alcotest.check cs __LOC__ early_secret1 (Hkdf.extract ~hash ~salt ikm) ;
  let t = Tls.Handshake_crypto13.(derive (empty cipher) ikm) in
  my_secret := Some t ;
  Alcotest.check cs __LOC__ early_secret1 t.secret

let ch_res_prefix = Cstruct.of_hex {|
01 00 01 fc 03 03 1b c3  ce b6 bb e3 9c ff 93 83
55 b5 a5 0a db 6d b2 1b  7a 6a f6 49 d7 b4 bc 41
9d 78 76 48 7d 95 00 00  06 13 01 13 03 13 02 01
00 01 cd 00 00 00 0b 00  09 00 00 06 73 65 72 76
65 72 ff 01 00 01 00 00  0a 00 14 00 12 00 1d 00
17 00 18 00 19 01 00 01  01 01 02 01 03 01 04 00
33 00 26 00 24 00 1d 00  20 e4 ff b6 8a c0 5f 8d
96 c9 9d a2 66 98 34 6c  6b e1 64 82 ba dd da fe
05 1a 66 b4 f1 8d 66 8f  0b 00 2a 00 00 00 2b 00
03 02 03 04 00 0d 00 20  00 1e 04 03 05 03 06 03
02 03 08 04 08 05 08 06  04 01 05 01 06 01 02 01
04 02 05 02 06 02 02 02  00 2d 00 02 01 01 00 1c
00 02 40 01 00 15 00 57  00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
29 00 dd 00 b8 00 b2 2c  03 5d 82 93 59 ee 5f f7
af 4e c9 00 00 00 00 26  2a 64 94 dc 48 6d 2c 8a
34 cb 33 fa 90 bf 1b 00  70 ad 3c 49 88 83 c9 36
7c 09 a2 be 78 5a bc 55  cd 22 60 97 a3 a9 82 11
72 83 f8 2a 03 a1 43 ef  d3 ff 5d d3 6d 64 e8 61
be 7f d6 1d 28 27 db 27  9c ce 14 50 77 d4 54 a3
66 4d 4e 6d a4 d2 9e e0  37 25 a6 a4 da fc d0 fc
67 d2 ae a7 05 29 51 3e  3d a2 67 7f a5 90 6c 5b
3f 7d 8f 92 f2 28 bd a4  0d da 72 14 70 f9 fb f2
97 b5 ae a6 17 64 6f ac  5c 03 27 2e 97 07 27 c6
21 a7 91 41 ef 5f 7d e6  50 5e 5b fb c3 88 e9 33
43 69 40 93 93 4a e4 d3  57 fa d6 aa cb
|}

let binder () =
  let binder_hash = Cstruct.of_hex "63 22 4b 2e 45 73 f2 d3 45 4c a8 4b 9d 00 9a 04 f6 be 9e 05 71 1a 83 96 47 3a ef a0 1e 92 4a 14" in
  Alcotest.check cs __LOC__ binder_hash (Mirage_crypto.Hash.digest hash ch_res_prefix) ;
  match !my_secret with
  | None -> Alcotest.fail "expected secret"
  | Some s ->
    let prk = Cstruct.of_hex "69 fe 13 1a 3b ba d5 d6 3c 64 ee bc c3 0e 39 5b 9d 81 07 72 6a 13 d0 74 e3 89 db c8 a4 e4 72 56" in
    Alcotest.check cs __LOC__ prk (Tls.Handshake_crypto13.derive_secret s "res binder" Cstruct.empty) ;
    let finished = Cstruct.of_hex "3a dd 4f b2 d8 fd f8 22 a0 ca 3c f7 67 8e f5 e8 8d ae 99 01 41 c5 92 4d 57 bb 6f a3 1b 9e 5f 9d" in
    Alcotest.check cs __LOC__ finished (Tls.Handshake_crypto13.finished hash prk ch_res_prefix)

let x25519 () =
  let c_priv = Cstruct.of_hex {|
49 af 42 ba 7f 79 94 85  2d 71 3e f2 78 4b cb ca
a7 91 1d e2 6a dc 56 42  cb 63 45 40 e7 ea 50 05
|}
  and c_keyshare = Cstruct.of_hex {|
99 38 1d e5 60 e4 bd 43  d2 3d 8e 43 5a 7d ba fe
b3 c0 6e 51 c1 3c ae 4d  54 13 69 1e 52 9a af 2c
|}
  and s_priv = Cstruct.of_hex {|
b1 58 0e ea df 6d d5 89  b8 ef 4f 2d 56 52 57 8c
c8 10 e9 98 01 91 ec 8d  05 83 08 ce a2 16 a2 1e
|}
  and s_keyshare = Cstruct.of_hex {|
c9 82 88 76 11 20 95 fe  66 76 2b db f7 c6 72 e1
56 d6 cc 25 3b 83 3d f1  dd 69 b1 b0 4e 75 1f 0f
|}
  in
  let check_pub pr pu =
    let _priv, pub = Hacl_x25519.gen_key ~rng:(fun _ -> pr) in
    Alcotest.check cs __LOC__ pu pub
  in
  let check_one p ks =
    let priv, _pub = Hacl_x25519.gen_key ~rng:(fun _ -> p) in
    match Hacl_x25519.key_exchange priv ks with
    | Ok shared -> Alcotest.check cs __LOC__ ikm shared
    | Error _ -> Alcotest.fail "bad kex"
  in
  check_one c_priv s_keyshare ;
  check_one s_priv c_keyshare ;
  check_pub c_priv c_keyshare ;
  check_pub s_priv s_keyshare

let tests = [
  "initial extract", `Quick, extract_secret_early ;
  "initial derive", `Quick, derive_hs_secret ;
  "handshake extract", `Quick, extract_handshake ;
  "derive c hs", `Quick, derive_c_hs_traffic ;
  "derive s hs", `Quick, derive_s_hs_traffic ;
  "derive finished", `Quick, derive_finished ;
  "derive master", `Quick, derive_master ;
  "extract master", `Quick, extract_master ;
  "derive hanshake keys", `Quick, derive_write_handshake_keys ;
  "derive traffic keys", `Quick, derive_traffic_keys ;
  "application write keys", `Quick, appdata_write ;
  "application read keys", `Quick, appdata_read ;
  "hs read keys", `Quick, derive_read_handshake_keys ;
  "resumption key", `Quick, resumption ;
  "payload", `Quick, payload_is_good ;
  "processed payload", `Quick, processed_payload ;
  "self signature", `Quick, self_signature ;
  "wire signature", `Quick, wire_signature ;
  "res secret", `Quick, res_secret ;
  "early resumed", `Quick, early1 ;
  "binder", `Quick, binder ;
  "x25519", `Quick, x25519 ;
]

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ()) ;
  Mirage_crypto_rng_unix.initialize () ;
  Alcotest.run "Key derivation tests" [ "key extraction and derivation", tests ]
