open OUnit2

open Mirage_crypto

open Test_common

(* NIST SP 800-38A test vectors for block cipher modes of operation *)

let nist_sp_800_38a = vx
  "6b c1 be e2 2e 40 9f 96 e9 3d 7e 11 73 93 17 2a
   ae 2d 8a 57 1e 03 ac 9c 9e b7 6f ac 45 af 8e 51
   30 c8 1c 46 a3 5c e4 11 e5 fb c1 19 1a 0a 52 ef
   f6 9f 24 45 df 4f 9b 17 ad 2b 41 7b e6 6c 37 10"

let aes_ecb_cases =
  let open Cipher_block in

  let case ~key ~out = (AES.ECB.of_secret (vx key), vx out)

  and check (key, out) _ =
    let enc = AES.ECB.encrypt ~key nist_sp_800_38a in
    let dec = AES.ECB.decrypt ~key enc in
    assert_cs_equal ~msg:"ciphertext" out enc ;
    assert_cs_equal ~msg:"plaintext" nist_sp_800_38a dec in

  cases_of check [
    case ~key: "2b 7e 15 16 28 ae d2 a6 ab f7 15 88 09 cf 4f 3c"
         ~out: "3a d7 7b b4 0d 7a 36 60 a8 9e ca f3 24 66 ef 97
                f5 d3 d5 85 03 b9 69 9d e7 85 89 5a 96 fd ba af
                43 b1 cd 7f 59 8e ce 23 88 1b 00 e3 ed 03 06 88
                7b 0c 78 5e 27 e8 ad 3f 82 23 20 71 04 72 5d d4"

  ; case ~key: "8e 73 b0 f7 da 0e 64 52 c8 10 f3 2b 80 90 79 e5
                62 f8 ea d2 52 2c 6b 7b"
         ~out: "bd 33 4f 1d 6e 45 f2 5f f7 12 a2 14 57 1f a5 cc
                97 41 04 84 6d 0a d3 ad 77 34 ec b3 ec ee 4e ef
                ef 7a fd 22 70 e2 e6 0a dc e0 ba 2f ac e6 44 4e
                9a 4b 41 ba 73 8d 6c 72 fb 16 69 16 03 c1 8e 0e"

  ; case ~key: "60 3d eb 10 15 ca 71 be 2b 73 ae f0 85 7d 77 81
                1f 35 2c 07 3b 61 08 d7 2d 98 10 a3 09 14 df f4"
         ~out: "f3 ee d1 bd b5 d2 a0 3c 06 4b 5a 7e 3d b1 81 f8
                59 1c cb 10 d4 10 ed 26 dc 5b a7 4a 31 36 28 70
                b6 ed 21 b9 9c a6 f4 f9 f1 53 e7 b1 be af ed 1d
                23 30 4b 7a 39 f9 f3 ff 06 7d 8d 8f 9e 24 ec c7"
  ]

let aes_cbc_cases =
  let open Cipher_block in

  let case ~key ~iv ~out = (AES.CBC.of_secret (vx key), vx iv, vx out)

  and check (key, iv, out) _ =
    let enc = AES.CBC.encrypt ~key ~iv nist_sp_800_38a in
    let dec = AES.CBC.decrypt ~key ~iv enc in
    assert_cs_equal ~msg:"ciphertext" out enc ;
    assert_cs_equal ~msg:"plaintext" nist_sp_800_38a dec in

  cases_of check [
    case ~key: "2b 7e 15 16 28 ae d2 a6 ab f7 15 88 09 cf 4f 3c"
         ~iv:  "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f"
         ~out: "76 49 ab ac 81 19 b2 46 ce e9 8e 9b 12 e9 19 7d
                50 86 cb 9b 50 72 19 ee 95 db 11 3a 91 76 78 b2
                73 be d6 b8 e3 c1 74 3b 71 16 e6 9e 22 22 95 16
                3f f1 ca a1 68 1f ac 09 12 0e ca 30 75 86 e1 a7"

  ; case ~key: "8e 73 b0 f7 da 0e 64 52 c8 10 f3 2b 80 90 79 e5
                62 f8 ea d2 52 2c 6b 7b"
         ~iv:  "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f"
         ~out: "4f 02 1d b2 43 bc 63 3d 71 78 18 3a 9f a0 71 e8
                b4 d9 ad a9 ad 7d ed f4 e5 e7 38 76 3f 69 14 5a
                57 1b 24 20 12 fb 7a e0 7f a9 ba ac 3d f1 02 e0
                08 b0 e2 79 88 59 88 81 d9 20 a9 e6 4f 56 15 cd"

  ; case ~key: "60 3d eb 10 15 ca 71 be 2b 73 ae f0 85 7d 77 81
                1f 35 2c 07 3b 61 08 d7 2d 98 10 a3 09 14 df f4"
         ~iv:  "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f"
         ~out: "f5 8c 4c 04 d6 e5 f1 ba 77 9e ab fb 5f 7b fb d6
                9c fc 4e 96 7e db 80 8d 67 9f 77 7b c6 70 2c 7d
                39 f2 33 69 a9 d9 ba cf a5 30 e2 63 04 23 14 61
                b2 eb 05 e2 c3 9b e9 fc da 6c 19 07 8c 6a 9d 1b"
  ]

let aes_ctr_cases =
  let case ~key ~ctr ~out ~ctr1 = test_case @@ fun _ ->
    let open Cipher_block.AES.CTR in
    let key  = vx key |> of_secret
    and ctr  = vx ctr |> ctr_of_cstruct
    and ctr1 = vx ctr1 |> ctr_of_cstruct
    and out  = vx out in
    let enc = encrypt ~key ~ctr nist_sp_800_38a in
    let dec = decrypt ~key ~ctr enc in
    assert_cs_equal ~msg:"cipher" out enc;
    assert_cs_equal ~msg:"plain" nist_sp_800_38a dec;
    let blocks = Cstruct.len nist_sp_800_38a / block_size in
    assert_equal ~msg:"counters" ctr1 (add_ctr ctr (Int64.of_int blocks))
  in
  [ case ~key:  "2b7e1516 28aed2a6 abf71588 09cf4f3c"
         ~ctr:  "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdfeff"
         ~out:  "874d6191 b620e326 1bef6864 990db6ce
                 9806f66b 7970fdff 8617187b b9fffdff
                 5ae4df3e dbd5d35e 5b4f0902 0db03eab
                 1e031dda 2fbe03d1 792170a0 f3009cee"
         ~ctr1: "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdff03"

  ; case ~key:  "8e73b0f7 da0e6452 c810f32b 809079e5
                 62f8ead2 522c6b7b"
         ~ctr:  "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdfeff"
         ~out:  "1abc9324 17521ca2 4f2b0459 fe7e6e0b
                 090339ec 0aa6faef d5ccc2c6 f4ce8e94
                 1e36b26b d1ebc670 d1bd1d66 5620abf7
                 4f78a7f6 d2980958 5a97daec 58c6b050"
         ~ctr1: "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdff03"

  ; case ~key:  "603deb10 15ca71be 2b73aef0 857d7781
                 1f352c07 3b6108d7 2d9810a3 0914dff4"
         ~ctr:  "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdfeff"
         ~out:  "601ec313 775789a5 b7a7f504 bbf3d228
                 f443e3ca 4d62b59a ca84e990 cacaf5c5
                 2b0930da a23de94c e87017ba 2d84988d
                 dfc9c58d b67aada6 13c2dd08 457941a6"
         ~ctr1: "f0f1f2f3 f4f5f6f7 f8f9fafb fcfdff03"

  ; case ~key:  "00010203 04050607 08090a0b 0c0d0e0f" (* ctr rollover *)
         ~ctr:  "00000000 00000000 ffffffff fffffffe"
         ~out:  "5d0a5645 378f579a 988ff186 d42eaa2f
                 978a655d 145bfe34 21656c8f 01101a43
                 23d0862c 47f7e3bf 95586ba4 2ab4cb31
                 790b0d01 93c0d022 3469534e 537ce82d"
         ~ctr1: "00000000 00000001 00000000 00000002"
  ]

(* aes gcm *)

let gcm_cases =
  let open Cipher_block in

  let case ~key ~p ~a ~iv ~c ~t =
    (AES.GCM.of_secret (vx key), vx p, vx a, vx iv, vx c, vx t) in

  let check (key, p, adata, iv, c, t) _ =
    let open AES.GCM in
    let { message = cdata ; tag = ctag } =
      AES.GCM.encrypt ~key ~iv ~adata p in
    let { message = pdata ; tag = ptag } =
      AES.GCM.decrypt ~key ~iv ~adata cdata
    in
    assert_cs_equal ~msg:"ciphertext" c cdata ;
    assert_cs_equal ~msg:"encryption tag" t ctag  ;
    assert_cs_equal ~msg:"decrypted plaintext" p pdata ;
    assert_cs_equal ~msg:"decryption tag" t ptag
  in

  cases_of check [

    case ~key: "00000000000000000000000000000000"
         ~p:   ""
         ~a:   ""
         ~iv:  "000000000000000000000000"
         ~c:   ""
         ~t:   "58e2fccefa7e3061367f1d57a4e7455a" ;
    case ~key: "00000000000000000000000000000000"
         ~p:   "00000000000000000000000000000000"
         ~a:   ""
         ~iv:  "000000000000000000000000"
         ~c:   "0388dace60b6a392f328c2b971b2fe78"
         ~t:   "ab6e47d42cec13bdf53a67b21257bddf" ;
    case ~key: "feffe9928665731c6d6a8f9467308308"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b391aafd255"
         ~a:   ""
         ~iv:  "cafebabefacedbaddecaf888"
         ~c:   "42831ec2217774244b7221b784d0d49c
                e3aa212f2c02a4e035c17e2329aca12e
                21d514b25466931c7d8f6a5aac84aa05
                1ba30b396a0aac973d58e091473f5985"
         ~t:   "4d5c2af327cd64a62cf35abd2ba6fab4" ;
    case ~key: "feffe9928665731c6d6a8f9467308308"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b39"
         ~a:   "feedfacedeadbeeffeedfacedeadbeef
                abaddad2"
         ~iv:  "cafebabefacedbaddecaf888"
         ~c:   "42831ec2217774244b7221b784d0d49c
                e3aa212f2c02a4e035c17e2329aca12e
                21d514b25466931c7d8f6a5aac84aa05
                1ba30b396a0aac973d58e091"
         ~t:   "5bc94fbc3221a5db94fae95ae7121a47" ;
    case ~key: "feffe9928665731c6d6a8f9467308308"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b39"
         ~a:   "feedfacedeadbeeffeedfacedeadbeef
                abaddad2"
         ~iv:  "cafebabefacedbad"
         ~c:   "61353b4c2806934a777ff51fa22a4755
                699b2a714fcdc6f83766e5f97b6c7423
                73806900e49f24b22b097544d4896b42
                4989b5e1ebac0f07c23f4598"
         ~t:   "3612d2e79e3b0785561be14aaca2fccb" ;
    case ~key: "feffe9928665731c6d6a8f9467308308"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b39"
         ~a:   "feedfacedeadbeeffeedfacedeadbeef
                abaddad2"
         ~iv:  "9313225df88406e555909c5aff5269aa
                6a7a9538534f7da1e4c303d2a318a728
                c3c0c95156809539fcf0e2429a6b5254
                16aedbf5a0de6a57a637b39b"
         ~c:   "8ce24998625615b603a033aca13fb894
                be9112a5c3a211a8ba262a3cca7e2ca7
                01e4a9a4fba43c90ccdcb281d48c7c6f
                d62875d2aca417034c34aee5"
         ~t:   "619cc5aefffe0bfa462af43c1699d050" ;
    case ~key: "feffe9928665731c6d6a8f9467308308
                feffe9928665731c"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b39"
         ~a:   "feedfacedeadbeeffeedfacedeadbeef
                abaddad2"
         ~iv:  "cafebabefacedbaddecaf888"
         ~c:   "3980ca0b3c00e841eb06fac4872a2757
                859e1ceaa6efd984628593b40ca1e19c
                7d773d00c144c525ac619d18c84a3f47
                18e2448b2fe324d9ccda2710"
         ~t:   "2519498e80f1478f37ba55bd6d27618c" ;
    case ~key: "feffe9928665731c6d6a8f9467308308
                feffe9928665731c6d6a8f9467308308"
         ~p:   "d9313225f88406e5a55909c5aff5269a
                86a7a9531534f7da2e4c303d8a318a72
                1c3c0c95956809532fcf0e2449a6b525
                b16aedf5aa0de657ba637b39"
         ~a:   "feedfacedeadbeeffeedfacedeadbeef
                abaddad2"
         ~iv:  "9313225df88406e555909c5aff5269aa
                6a7a9538534f7da1e4c303d2a318a728
                c3c0c95156809539fcf0e2429a6b5254
                16aedbf5a0de6a57a637b39b"
         ~c:   "5a8def2f0c9e53f1f75d7853659e2a20
                eeb2b22aafde6419a058ab4f6f746bf4
                0fc0c3b780f244452da3ebf1c5d82cde
                a2418997200ef82e44ae7e3f"
         ~t:   "a44a8266ee1c8eb0c8b5d4cf5ae9f19a";
    case ~key: "00000000000000000000000000000000"  (* large GHASH batch *)
         ~p:   ""
         ~a:   "f0f0f0f0f0f0f0f00f0f0f0f0f0f0f0f
                e0e0e0e0e0e0e0e00e0e0e0e0e0e0e0e
                d0d0d0d0d0d0d0d00d0d0d0d0d0d0d0d
                c0c0c0c0c0c0c0c00c0c0c0c0c0c0c0c
                b0b0b0b0b0b0b0b00b0b0b0b0b0b0b0b
                a0a0a0a0a0a0a0a00a0a0a0a0a0a0a0a
                90909090909090900909090909090909
                80808080808080800808080808080808
                70707070707070700707070707070707
                60606060606060600606060606060606
                50505050505050500505050505050505
                40404040404040400404040404040404
                30303030303030300303030303030303
                20202020202020200202020202020202
                10101010101010100101010101010101
                00000000000000000000000000000000
                ff"
         ~iv:  "000000000000000000000000"
         ~c:   ""
         ~t:   "9bfdb8fdac1be65739780c41703c0fb6";
    case ~key: "00000000000000000000000000000002"  (* ctr rollover *)
         ~iv:  "3222415d"
         ~p:   "deadbeefdeadbeefdeadbeefdeadbeef
                deadbeefdeadbeefdeadbeefdeadbeef
                deadbeef"
         ~a:   ""
         ~c:   "42627ce3de61b5c105c7f01629c031c1
                b890bb273b6b6bc26b56c801f87fa95c
                a8b37503"
         ~t:   "3631cbe44782713b93b1c7d93c3c8638"
]


(* from SP800-38C_updated-July20_2007.pdf appendix C *)
let ccm_cases =
  let open Cipher_block.AES.CCM in
  let case ~key ~p ~a ~nonce ~c ~maclen =
    (of_secret ~maclen (vx key), vx p, vx a, vx nonce, vx c) in

  let check (key, p, adata, nonce, c) _ =
    let cip = encrypt ~key ~nonce ~adata p in
    assert_cs_equal ~msg:"encrypt" c cip ;
    match decrypt ~key ~nonce ~adata c with
      | Some x -> assert_cs_equal ~msg:"decrypt" p x
      | None -> assert_failure "decryption broken"
  in

  cases_of check [

    case ~key:    "404142434445464748494a4b4c4d4e4f"
         ~p:      "20212223"
         ~a:      "0001020304050607"
         ~nonce:  "10111213141516"
         ~c:      "7162015b4dac255d"
         ~maclen: 4
    ;
    case ~key:    "40414243 44454647 48494a4b 4c4d4e4f"
         ~p:      "20212223 24252627 28292a2b 2c2d2e2f"
         ~a:      "00010203 04050607 08090a0b 0c0d0e0f"
         ~nonce:  "10111213 14151617"
         ~c:      "d2a1f0e0 51ea5f62 081a7792 073d593d 1fc64fbf accd"
         ~maclen: 6
    ;
    case ~key:    "404142434445464748494a4b4c4d4e4f"
         ~p:      "202122232425262728292a2b2c2d2e2f3031323334353637"
         ~a:      "000102030405060708090a0b0c0d0e0f10111213"
         ~nonce:  "101112131415161718191a1b"
         ~c:      "e3b201a9f5b71a7a9b1ceaeccd97e70b6176aad9a4428aa5484392fbc1b09951"
         ~maclen: 8
  ]

let ccm_regressions =
  let open Cipher_block.AES.CCM in
  let no_vs_empty_ad _ =
    (* as reported in https://github.com/mirleft/ocaml-nocrypto/issues/166 *)
    (* see RFC 3610 Section 2.1, AD of length 0 should be same as no AD *)
    let key = of_secret ~maclen:16 (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "0001020304050607"
    and plaintext = Cstruct.of_string "hello"
    in
    assert_cs_equal ~msg:"CCM no vs empty ad"
      (encrypt ~key ~nonce plaintext)
      (encrypt ~adata:Cstruct.empty ~key ~nonce plaintext)
  and short_nonce_enc _ =
    (* as reported in https://github.com/mirleft/ocaml-nocrypto/issues/167 *)
    (* valid nonce sizes for CCM are 7..13 (L can be 2..8, nonce is 15 - L)*)
    (* see RFC3610 Section 2.1 *)
    let key = of_secret ~maclen:16 (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = Cstruct.empty
    and plaintext = Cstruct.of_string "hello"
    in
    assert_raises ~msg:"CCM with short nonce raises"
      (Invalid_argument "Mirage_crypto: CCM: nonce length not between 7 and 13: 0")
      (fun () -> encrypt ~key ~nonce plaintext)
  and short_nonce_enc2 _ =
    let key = of_secret ~maclen:16 (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "00"
    and plaintext = Cstruct.of_string "hello"
    in
    assert_raises ~msg:"CCM with short nonce raises"
      (Invalid_argument "Mirage_crypto: CCM: nonce length not between 7 and 13: 1")
      (fun () -> encrypt ~key ~nonce plaintext)
  and short_nonce_enc3 _ =
    let key = of_secret ~maclen:16 (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "000102030405"
    and plaintext = Cstruct.of_string "hello"
    in
    assert_raises ~msg:"CCM with short nonce raises"
      (Invalid_argument "Mirage_crypto: CCM: nonce length not between 7 and 13: 6")
      (fun () -> encrypt ~key ~nonce plaintext)
  and long_nonce_enc _ =
    let key = of_secret ~maclen:16 (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "000102030405060708090a0b0c0d"
    and plaintext = Cstruct.of_string "hello"
    in
    assert_raises ~msg:"CCM with short nonce raises"
      (Invalid_argument "Mirage_crypto: CCM: nonce length not between 7 and 13: 14")
      (fun () -> encrypt ~key ~nonce plaintext)
  and enc_dec_empty_message _ =
    (* as reported in https://github.com/mirleft/ocaml-nocrypto/issues/168 *)
    let key = of_secret ~maclen:16 (vx "000102030405060708090a0b0c0d0e0f")
    and nonce = vx "0001020304050607"
    and adata = Cstruct.of_string "hello"
    and p = Cstruct.empty
    in
    let cipher = encrypt ~adata ~key ~nonce p in
    match decrypt ~key ~nonce ~adata cipher with
    | Some x -> assert_cs_equal ~msg:"CCM decrypt of empty message" p x
    | None -> assert_failure "decryption broken"
  in
  [
    test_case no_vs_empty_ad ;
    test_case short_nonce_enc ;
    test_case short_nonce_enc2 ;
    test_case short_nonce_enc3 ;
    test_case long_nonce_enc ;
    test_case enc_dec_empty_message ;
  ]

let gcm_regressions =
  let open Cipher_block.AES.GCM in
  let msg = vx "000102030405060708090a0b0c0d0e0f" in
  let key = of_secret msg
  and iv = Cstruct.empty
  in
  let iv_zero_length_enc _ =
    (* reported in https://github.com/mirleft/ocaml-nocrypto/issues/169 *)
    assert_raises ~msg:"GCM with iv of length 0"
      (Invalid_argument "Mirage_crypto: GCM: invalid IV of length 0")
      (fun () -> encrypt ~key ~iv msg)
  and iv_zero_length_dec _ =
    assert_raises ~msg:"GCM with iv of 0"
      (Invalid_argument "Mirage_crypto: GCM: invalid IV of length 0")
      (fun () -> decrypt ~key ~iv msg)
  in
  [
    test_case iv_zero_length_enc ;
    test_case iv_zero_length_dec ;
  ]


let suite = [
  "AES-ECB" >::: [ "SP 300-38A" >::: aes_ecb_cases ] ;
  "AES-CBC" >::: [ "SP 300-38A" >::: aes_cbc_cases ] ;
  "AES-CTR" >::: [ "SP 300-38A" >::: aes_ctr_cases; ] ;
  "AES-GCM" >::: gcm_cases ;
  "AES-CCM" >::: ccm_cases ;
  "AES-CCM-REGRESSION" >::: ccm_regressions ;
  "AES-GCM-REGRESSION" >::: gcm_regressions ;
]
