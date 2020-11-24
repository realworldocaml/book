open OUnit2

open Mirage_crypto

open Test_common

let f1_blk_eq ?msg ?(n=1) f (x, y) _ =
  let xs = blocks_of_cs n (vx x) in
  assert_cs_equal ?msg (f (iter_list xs)) (vx y)

let hash_cases (m : (module Hash.S)) ~hash =
  let module H = ( val m ) in
  [ "digest"  >::: cases_of (f1_eq H.digest) hash ;
    "digesti" >::: cases_of (f1_blk_eq H.digesti) hash ;
  ]

let hash_cases_mac (m : (module Hash.S)) ~hash ~mac =
  let module H = ( val m ) in
  [ "digest"  >::: cases_of (f1_eq H.digest) hash ;
    "digesti" >::: cases_of (f1_blk_eq H.digesti) hash ;
    "hmac"    >::: cases_of (f2_eq (fun key -> H.hmac ~key)) mac ;
  ]

(* MD5 *)

let md5_cases =
  hash_cases_mac ( module Hash.MD5 )
  ~hash:[
    "" ,
    "d4 1d 8c d9 8f 00 b2 04 e9 80 09 98 ec f8 42 7e" ;

    "00",
    "93 b8 85 ad fe 0d a0 89 cd f6 34 90 4f d5 9f 71" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f" ,
    "1a c1 ef 01 e9 6c af 1b e0 d3 29 33 1a 4f c2 a8" ;
  ]
  ~mac:[
    "2c 03 ca 51 71 a3 d2 d1 41 71 79 6f c8 b2 6c 54" ,
    "8b bb 87 f4 76 4f ba 6a 55 61 c9 80 d5 35 58 4f
     0a 96 cb 60 49 2b 6e dd 71 a1 1e e5 7a 78 9b 73" ,
    "05 8b 08 41 09 79 8b 56 3d 81 49 1f 5f 82 5b ba" ;

    "2c 03 ca 51 71 a3 d2 d1 41 71 79 6f c8 b2 6c 54
     f0 0d a1 07 6c c9 e4 1f b2 17 ec ad 88 56 a2 6e
     d7 83 c3 3d 85 99 0d 8d c5 8d 03 50 00 e2 6e 80
     0c b5 9a 00 26 fd 15 fd 4c e1 84 9d a5 c6 fa a8
     f7 ef f6 c8 76 73 a3 47 0a d5 5a 5b 56 49 22 ec" ,
    "8b bb 87 f4 76 4f ba 6a 55 61 c9 80 d5 35 58 4f
     0a 96 cb 60 49 2b 6e dd 71 a1 1e e5 7a 78 9b 73" ,
    "61 ac 5c 29 9f e2 18 95 d5 4b eb ff 60 42 91 df" ;

    "2c 03 ca 51 71 a3 d2 d1 41 71 79 6f c8 b2 6c 54
     f0 0d a1 07 6c c9 e4 1f b2 17 ec ad 88 56 a2 6e
     d7 83 c3 3d 85 99 0d 8d c5 8d 03 50 00 e2 6e 80
     0c b5 9a 00 26 fd 15 fd 4c e1 84 9d a5 c6 fa a8" ,
    "8b bb 87 f4 76 4f ba 6a 55 61 c9 80 d5 35 58 4f
     0a 96 cb 60 49 2b 6e dd 71 a1 1e e5 7a 78 9b 73" ,
    "ce 44 c2 a1 c5 46 a7 08 a4 0a 7c f2 5e af b1 33" ;
  ]

(* SHA *)

let sha1_cases =
  hash_cases_mac ( module Hash.SHA1 )
  ~hash:[
    "" ,
    "da 39 a3 ee 5e 6b 4b 0d 32 55 bf ef 95 60 18 90
     af d8 07 09" ;

    "00" ,
    "5b a9 3c 9d b0 cf f9 3f 52 b5 21 d7 42 0e 43 f6
     ed a2 78 4f" ;

    "89 d1 68 64 8d 06 0c f2 ed a1 9a a3 10 56 85 48
     69 84 63 df 13 7c 96 5e b5 7b 23 ec b1 f8 e9 ef" ,
    "00 6f 23 b3 5d 7d 09 78 03 35 68 97 ea 6e e3 3c
     57 b2 11 ca" ;
  ]
  ~mac:[
    "", "",
    "fb db 1d 1b 18 aa 6c 08 32 4b 7d 64 b7 1f b7 63
     70 69 0e 1d" ;

    "9c 64 fc 6a 9a bb 1e 04 43 6d 58 49 3f 0d 30 21
     d6 8f eb a9 67 c0 1f 9f c9 35 dc a5 95 9b 6c 07
     4b 09 c0 39 bb c6 dc da 97 aa c8 ea 88 4e 17 e9
     7c c6 d9 f7 73 70 e0 cb 1d 64 de 6d 57 91 31 b3" ,
    "",
    "f9 b1 39 0f 1d 88 09 1b 1d a4 4a d5 d6 33 28 65
     c2 70 ca da";

    "9c 64 fc 6a 9a bb 1e 04 43 6d 58 49 3f 0d 30 21
     d6 8f eb a9 67 c0 1f 9f c9 35 dc a5 95 9b 6c 07
     4b 09 c0 39 bb c6 dc da 97 aa c8 ea 88 4e 17 e9
     7c c6 d9 f7 73 70 e0 cb 1d 64 de 6d 57 91 31 b3" ,
    "0d 83 e2 e9 b3 98 e2 8b ea e0 59 7f 37 15 95 1a
     4b 4c 3c ce 4b de 15 4f 53 da fb 2f b4 9f 03 ea" ,
    "ca 02 cd 56 77 dc b5 c1 3e de da 34 51 d9 e2 5c
     d9 29 4c 53" ;

    "9c 64 fc 6a 9a bb 1e 04 43 6d 58 49 3f 0d 30 21
     d6 8f eb a9 67 c0 1f 9f c9 35 dc a5 95 9b 6c 07
     4b 09 c0 39 bb c6 dc da 97 aa c8 ea 88 4e 17 e9
     7c c6 d9 f7 73 70 e0 cb 1d 64 de 6d 57 91 31 b3
     8e 17 5f 4e de 38 f4 14 48 bc 74 56 05 7a 3c 3b" ,
    "0d 83 e2 e9 b3 98 e2 8b ea e0 59 7f 37 15 95 1a
     4b 4c 3c ce 4b de 15 4f 53 da fb 2f b4 9f 03 ea" ,
    "7f f9 d5 9e 62 e8 d7 13 91 9f a2 a7 be 64 85 c5
     a0 39 ec 04";
  ]

let sha224_cases =
  hash_cases (module Hash.SHA224)
  ~hash:[
    "" ,
    "d1 4a 02 8c 2a 3a 2b c9 47 61 02 bb 28 82 34 c4
     15 a2 b0 1f 82 8e a6 2a c5 b3 e4 2f" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f" ,
    "52 9d 65 6a 8b c4 13 fe f5 8d a8 2e 1b f0 30 8d
     cf e0 42 9d cd 80 68 7e 69 c9 46 33" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
     10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f
     20 21 22 23 24 25 26 27 28 29 2a 2b 2c 2d 2e 2f
     30 31 32 33 34 35 36 37 38 39 3a 3b 3c 3d 3e 3f",
    "c3 7b 88 a3 52 2d bf 7a c3 0d 1c 68 ea 39 7a c1
     1d 47 73 57 1a ed 01 dd ab 73 53 1e" ;
  ]

let sha256_cases =
  hash_cases (module Hash.SHA256)
  ~hash:[
    "" ,
    "e3 b0 c4 42 98 fc 1c 14 9a fb f4 c8 99 6f b9 24
     27 ae 41 e4 64 9b 93 4c a4 95 99 1b 78 52 b8 55" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f" ,
    "be 45 cb 26 05 bf 36 be bd e6 84 84 1a 28 f0 fd
     43 c6 98 50 a3 dc e5 fe db a6 99 28 ee 3a 89 91" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
     10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f
     20 21 22 23 24 25 26 27 28 29 2a 2b 2c 2d 2e 2f
     30 31 32 33 34 35 36 37 38 39 3a 3b 3c 3d 3e 3f",
    "fd ea b9 ac f3 71 03 62 bd 26 58 cd c9 a2 9e 8f
     9c 75 7f cf 98 11 60 3a 8c 44 7c d1 d9 15 11 08"
  ]

let sha384_cases =
  hash_cases (module Hash.SHA384)
  ~hash:[
    "" ,
    "38 b0 60 a7 51 ac 96 38 4c d9 32 7e b1 b1 e3 6a
     21 fd b7 11 14 be 07 43 4c 0c c7 bf 63 f6 e1 da
     27 4e de bf e7 6f 65 fb d5 1a d2 f1 48 98 b9 5b" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f" ,
    "c8 1d f9 8d 9e 6d e9 b8 58 a1 e6 eb a0 f1 a3 a3
     99 d9 8c 44 1e 67 e1 06 26 01 80 64 85 bb 89 12
     5e fd 54 cc 78 df 5f bc ea bc 93 cd 7c 7b a1 3b" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
     10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f
     20 21 22 23 24 25 26 27 28 29 2a 2b 2c 2d 2e 2f
     30 31 32 33 34 35 36 37 38 39 3a 3b 3c 3d 3e 3f
     40 41 42 43 44 45 46 47 48 49 4a 4b 4c 4d 4e 4f
     50 51 52 53 54 55 56 57 58 59 5a 5b 5c 5d 5e 5f
     60 61 62 63 64 65 66 67 68 69 6a 6b 6c 6d 6e 6f
     70 71 72 73 74 75 76 77 78 79 7a 7b 7c 7d 7e 7f" ,
    "ca 23 85 77 33 19 12 45 34 11 1a 36 d0 58 1f c3
     f0 08 15 e9 07 03 4b 90 cf f9 c3 a8 61 e1 26 a7
     41 d5 df cf f6 5a 41 7b 6d 72 96 86 3a c0 ec 17"
  ]


let sha512_cases =
  hash_cases (module Hash.SHA512)
  ~hash:[
    "" ,
    "cf 83 e1 35 7e ef b8 bd f1 54 28 50 d6 6d 80 07
     d6 20 e4 05 0b 57 15 dc 83 f4 a9 21 d3 6c e9 ce
     47 d0 d1 3c 5d 85 f2 b0 ff 83 18 d2 87 7e ec 2f
     63 b9 31 bd 47 41 7a 81 a5 38 32 7a f9 27 da 3e" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f" ,
    "da a2 95 be ed 4e 2e e9 4c 24 01 5b 56 af 62 6b
     4f 21 ef 9f 44 f2 b3 d4 0f c4 1c 90 90 0a 6b f1
     b4 86 7c 43 c5 7c da 54 d1 b6 fd 48 69 b3 f2 3c
     ed 5e 0b a3 c0 5d 0b 16 80 df 4e c7 d0 76 24 03" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
     10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f
     20 21 22 23 24 25 26 27 28 29 2a 2b 2c 2d 2e 2f
     30 31 32 33 34 35 36 37 38 39 3a 3b 3c 3d 3e 3f
     40 41 42 43 44 45 46 47 48 49 4a 4b 4c 4d 4e 4f
     50 51 52 53 54 55 56 57 58 59 5a 5b 5c 5d 5e 5f
     60 61 62 63 64 65 66 67 68 69 6a 6b 6c 6d 6e 6f
     70 71 72 73 74 75 76 77 78 79 7a 7b 7c 7d 7e 7f" ,
    "1d ff d5 e3 ad b7 1d 45 d2 24 59 39 66 55 21 ae
     00 1a 31 7a 03 72 0a 45 73 2b a1 90 0c a3 b8 35
     1f c5 c9 b4 ca 51 3e ba 6f 80 bc 7b 1d 1f da d4
     ab d1 34 91 cb 82 4d 61 b0 8d 8c 0e 15 61 b3 f7" ;
  ]

let suite = [
  "MD5" >::: md5_cases ;
  "SHA1" >::: sha1_cases ;
  "sha224" >::: sha224_cases ;
  "sha256" >::: sha256_cases ;
  "sha384" >::: sha384_cases ;
  "sha512" >::: sha512_cases ;
]
