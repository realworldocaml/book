open! Import
open! Nativeint

let%expect_test "hash coherence" =
  check_int_hash_coherence [%here] (module Nativeint);
  [%expect {| |}]
;;

type test_case = nativeint * int32 * int64

let test_cases : test_case list =
  [ 0x0000_0011n, 0x1100_0000l, 0x1100_0000_0000_0000L
  ; 0x0000_1122n, 0x2211_0000l, 0x2211_0000_0000_0000L
  ; 0x0011_2233n, 0x3322_1100l, 0x3322_1100_0000_0000L
  ; 0x1122_3344n, 0x4433_2211l, 0x4433_2211_0000_0000L
  ]
;;

let%expect_test "bswap native" =
  List.iter test_cases ~f:(fun (arg, bswap_int32, bswap_int64) ->
    let result = bswap arg in
    match Sys.word_size_in_bits with
    | 32 -> assert (Int32.equal bswap_int32 (Nativeint.to_int32_trunc result))
    | 64 -> assert (Int64.equal bswap_int64 (Nativeint.to_int64 result))
    | _ -> assert false)
;;
