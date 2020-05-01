open OUnit2

open Mirage_crypto.Uncommon
open Mirage_crypto_pk

open Test_common

let n_encode_decode_selftest ~typ ~bound n =
  typ ^ "selftest" >:: times ~n @@ fun _ ->
    let r = Z_extra.gen bound in
    let s = Z_extra.(of_cstruct_be @@ to_cstruct_be r)
    and t = Z_extra.(of_cstruct_be @@ to_cstruct_be ~size:24 r) in
    assert_equal r s;
    assert_equal r t

let n_decode_reencode_selftest ~typ ~bytes n =
  typ ^ " selftest" >:: times ~n @@ fun _ ->
    let cs  = Mirage_crypto_rng.generate bytes in
    let cs' = Z_extra.(to_cstruct_be ~size:bytes @@ of_cstruct_be cs) in
    assert_cs_equal cs cs'

let random_n_selftest ~typ n bounds =
  typ ^ " selftest" >::: (
    bounds |> List.map @@ fun (lo, hi) ->
      "selftest" >:: times ~n @@ fun _ ->
        let x = Z_extra.gen_r lo hi in
        if x < lo || x >= hi then assert_failure "range error"
  )

let int_safe_bytes = Sys.word_size // 8 - 1

let suite = [
  "Numeric extraction 1" >::: [
    n_encode_decode_selftest
      ~typ:"z" ~bound:Z.(of_int64 Int64.max_int) 2000 ;
  ] ;

  "Numeric extraction 2" >::: [
    n_decode_reencode_selftest ~typ:"z" ~bytes:37 2000 ;
  ];

  "RNG extraction" >::: [
    random_n_selftest ~typ:"Z" 1000 [
      Z.(of_int 7, of_int 135);
      Z.(of_int 0, of_int 536870913);
      Z.(of_int 0, of_int64 2305843009213693953L)
    ] ;
  ]
]
