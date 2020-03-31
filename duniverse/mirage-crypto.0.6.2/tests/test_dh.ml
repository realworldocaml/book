open OUnit2

open Mirage_crypto_pk

open Test_common

let dh_selftest ~bits n =
  "selftest" >:: times ~n @@ fun _ ->
    let p = Dh.gen_group ~bits () in
    let (s1, m1) = Dh.gen_key p
    and (s2, m2) = Dh.gen_key p in
    let sh1 = Dh.shared s1 m2
    and sh2 = Dh.shared s2 m1 in
    assert_equal sh1 sh2
      ~cmp:(eq_opt Cstruct.equal)
      ~pp_diff:(pp_diff (pp_opt Cstruct.hexdump_pp))
      ~msg:"shared secret"

let dh_shared_0 =
  "shared_0" >:: fun _ ->
    let gy = vx
        "14 ac e2 c0 9c c0 0c 25 89 71 b2 d0 1c 94 58 21
         02 23 b7 23 ec 3e 24 e5 a3 c2 fd 16 cc 49 f0 e2
         87 62 a5 a0 73 f5 de 5b 9b eb c3 60 0b a4 03 38
         0f e1 8c f2 80 b3 64 16 f2 af ab 2e ec 25 81 2c
         84 ae 92 0a 0f 15 9b f3 d9 1f dc 08 7d 8d 27 3a
         91 7d a5 89 dc 94 d6 bc 3f 9d 6d b3 f8 8e f2 37
         86 54 ec 85 ea 4c a0 4c b1 f6 49 83 1c 62 a7 79
         2b 8b 9c e7 fa 47 3e 34 6c c5 ae 12 a3 4e d5 ce
         4b da ea 72 7a 8d c6 67 ef 7e f2 00 24 d7 21 42
         a5 23 69 38 7e ec b5 fc 4b 89 42 c4 32 fa e5 58
         6f 39 5d a7 4e cd b5 da dc 1e 52 fe a4 33 72 c1
         82 48 8a 5b c1 44 bc 60 9b 38 5b 80 5f 44 14 93"
    and s = vx
        "f9 47 87 95 d2 a1 6d d1 7c c8 a9 c0 71 28 a2 82
         71 95 7e 79 87 0b fc 34 a2 42 ec 42 ac cc 42 81
         7b f6 c4 f5 80 a9 70 e3 35 93 9b a3 21 81 a4 e3
         6b 65 3f 1c 5c ab 87 23 86 eb 76 29 66 26 5b e9
         c4 d0 26 05 3f de 6c 2f a6 14 f6 bf 77 74 a0 e8
         ef e7 12 62 a3 83 e5 66 d8 6c e5 c6 58 67 2a 61
         f5 7b 7c 15 15 63 22 55 96 92 9e bd cc b3 bc 2b
         5e e1 ac 5f 75 23 ca 2f 19 5a f1 18 6e 17 f8 c2
         f7 11 c7 14 1d 81 bd be 02 31 3f 49 62 7d 02 11
         29 22 63 6e bb 1a 7f 93 bd 98 db 20 94 f8 f0 2e
         db ce 9d 79 db b9 a7 41 5f e5 29 a2 31 f8 e2 c3
         30 6a 09 f2 16 a7 30 8c 2f 36 7b 71 99 1e 28 54"
    and shared = vx
        "a7 40 0d eb f0 4b 2b ec cb 90 3c 55 2d 3c 17 63
         b2 4b 4e 1a ff 1e a0 24 c6 56 e3 5e 44 7b d0 01
         ef b3 6b 57 20 0e 15 95 b1 53 1a 83 16 3a b1 61
         06 65 f1 7e 64 63 6f 23 86 22 34 c3 fe a9 60 87
         3f 18 c6 5d 44 3e ac e3 85 34 86 6f db aa 31 3b
         4b 4d 68 f7 19 d7 91 a3 12 27 d6 5a ce 29 c8 1b
         5a 59 74 10 8c ff 98 4e 4f 37 ef 5b 43 e8 e2 ad
         a8 49 c9 7e c3 c5 3d 35 40 30 8e a4 41 69 1d 16
         34 ba 9a 7e f3 ab d1 0e bb f2 81 15 e9 04 63 ee
         1b bf cc 24 6d cb 41 c4 06 b2 f3 01 1b 31 3a 1e
         dc e3 3b c7 cc 1d 19 95 d9 fe 6a 5c a7 57 46 dd
         84 69 0c 45 37 2e 1f 52 96 05 d7 e5 01 9a c8"
    in
    let grp = Dh.Group.oakley_5 in
    match Dh.(shared (fst (key_of_secret grp ~s)) gy) with
    | None -> assert_failure "degenerate shared secret"
    | Some shared' ->
        assert_cs_equal ~msg:"shared secret" shared shared'

let suite = [
  dh_selftest ~bits:16  1000 ;
  dh_selftest ~bits:128 100  ;
  dh_shared_0
]
