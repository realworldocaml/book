open! Import
open! Word_size

let%expect_test _ =
  print_s [%message (W32 : t)];
  [%expect {|
    (W32 W32) |}];
  print_s [%message (W64 : t)];
  [%expect {|
    (W64 W64) |}];
;;
