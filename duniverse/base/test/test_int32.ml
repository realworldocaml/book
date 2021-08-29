open! Import
open! Int32

let%expect_test "hash coherence" =
  check_int_hash_coherence [%here] (module Int32);
  [%expect {| |}]
;;

let numbers = [ 0x10_20l; 0x11_22_33l; 0x11_22_33_1Fl; 0x11_22_33_44l ]
let test = test_conversion ~to_string:(fun x -> Int32.Hex.to_string_hum x)

let%expect_test "bswap16" =
  List.iter numbers ~f:(test bswap16);
  [%expect
    {|
    0x1020 --> 0x2010
    0x11_2233 --> 0x3322
    0x1122_331f --> 0x1f33
    0x1122_3344 --> 0x4433 |}]
;;

let%expect_test "bswap32" =
  List.iter numbers ~f:(test bswap32);
  [%expect
    {|
   0x1020 --> 0x2010_0000
   0x11_2233 --> 0x3322_1100
   0x1122_331f --> 0x1f33_2211
   0x1122_3344 --> 0x4433_2211 |}]
;;
