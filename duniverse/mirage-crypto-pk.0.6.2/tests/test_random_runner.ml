open OUnit2

open Mirage_crypto

open Test_common
open Test_common_random

(* randomized selfies *)

let ecb_selftest (m : (module Cipher_block.S.ECB)) n =
  let module C = ( val m ) in
  "selftest" >:: times ~n @@ fun _ ->
    let data  = Mirage_crypto_rng.generate (C.block_size * 8)
    and key   = C.of_secret @@ Mirage_crypto_rng.generate (sample C.key_sizes) in
    let data' =
      C.( data |> encrypt ~key |> encrypt ~key
               |> decrypt ~key |> decrypt ~key ) in
    assert_cs_equal ~msg:"ecb mismatch" data data'

let cbc_selftest (m : (module Cipher_block.S.CBC)) n  =
  let module C = ( val m ) in
  "selftest" >:: times ~n @@ fun _ ->
    let data = Mirage_crypto_rng.generate (C.block_size * 8)
    and iv   = Mirage_crypto_rng.generate C.block_size
    and key  = C.of_secret @@ Mirage_crypto_rng.generate (sample C.key_sizes) in
    assert_cs_equal ~msg:"CBC e->e->d->d" data
      C.( data |> encrypt ~key ~iv |> encrypt ~key ~iv
               |> decrypt ~key ~iv |> decrypt ~key ~iv );
    let (d1, d2) = Cstruct.split data (C.block_size * 4) in
    assert_cs_equal ~msg:"CBC chain"
      C.(encrypt ~key ~iv data)
      C.( let e1 = encrypt ~key ~iv d1 in
          Cstruct.append e1 (encrypt ~key ~iv:(C.next_iv ~iv e1) d2) )

let ctr_selftest (m : (module Cipher_block.S.CTR)) n =
  let module M = (val m) in
  let bs = M.block_size in
  "selftest" >:: times ~n @@ fun _ ->
    let key  = M.of_secret @@ Mirage_crypto_rng.generate (sample M.key_sizes)
    and ctr  = Mirage_crypto_rng.generate bs |> M.ctr_of_cstruct
    and data = Mirage_crypto_rng.(generate @@ bs + Randomconv.int ~bound:(20 * bs) Mirage_crypto_rng.generate) in
    let enc = M.encrypt ~key ~ctr data in
    let dec = M.decrypt ~key ~ctr enc in
    assert_cs_equal ~msg:"CTR e->d" data dec;
    let (d1, d2) =
      Cstruct.split data @@ bs * Randomconv.int ~bound:(Cstruct.len data / bs) Mirage_crypto_rng.generate in
    assert_cs_equal ~msg:"CTR chain" enc @@
      Cstruct.append (M.encrypt ~key ~ctr d1)
                     (M.encrypt ~key ~ctr:(M.next_ctr ~ctr d1) d2)

let ctr_offsets (type c) ~zero (m : (module Cipher_block.S.CTR with type ctr = c)) n =
  let module M = (val m) in
  "offsets" >:: fun _ ->
    let key = M.of_secret @@ Mirage_crypto_rng.generate M.key_sizes.(0) in
    for i = 0 to n - 1 do
      let ctr = match i with
        | 0 -> M.add_ctr zero (-1L)
        | _ -> Mirage_crypto_rng.generate M.block_size |> M.ctr_of_cstruct
      and gap = Randomconv.int ~bound:64 Mirage_crypto_rng.generate in
      let s1 = M.stream ~key ~ctr ((gap + 1) * M.block_size)
      and s2 = M.stream ~key ~ctr:(M.add_ctr ctr (Int64.of_int gap)) M.block_size in
      assert_cs_equal ~msg:"shifted stream"
        Cstruct.(sub s1 (gap * M.block_size) M.block_size) s2
    done

let xor_selftest n =
  "selftest" >:: times ~n @@ fun _ ->

    let n         = Randomconv.int ~bound:30 Mirage_crypto_rng.generate in
    let (x, y, z) = Mirage_crypto_rng.(generate n, generate n, generate n) in

    let xyz  = Uncommon.Cs.(xor (xor x y) z)
    and xyz' = Uncommon.Cs.(xor x (xor y z)) in
    let x1   = Uncommon.Cs.(xor xyz (xor y z))
    and x2   = Uncommon.Cs.(xor (xor z y) xyz) in

    assert_cs_equal ~msg:"assoc" xyz xyz' ;
    assert_cs_equal ~msg:"invert" x x1 ;
    assert_cs_equal ~msg:"commut" x1 x2

let suite =
  "All" >::: [
    "XOR" >::: [ xor_selftest 300 ] ;
    "3DES-ECB" >::: [ ecb_selftest (module Cipher_block.DES.ECB) 100 ] ;

    "3DES-CBC" >::: [ cbc_selftest (module Cipher_block.DES.CBC) 100 ] ;

    "3DES-CTR" >::: Cipher_block.[ ctr_selftest (module DES.CTR) 100;
                                   ctr_offsets  (module DES.CTR) 100 ~zero:0L; ] ;

    "AES-ECB" >::: [ ecb_selftest (module Cipher_block.AES.ECB) 100 ] ;
    "AES-CBC" >::: [ cbc_selftest (module Cipher_block.AES.CBC) 100 ] ;
    "AES-CTR" >::: Cipher_block.[ ctr_selftest (module AES.CTR) 100;
                                  ctr_offsets  (module AES.CTR) 100 ~zero:(0L, 0L) ] ;

  ]

let () =
  Mirage_crypto_rng_unix.initialize ();
  run_test_tt_main suite
