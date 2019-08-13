open! Base
open! Import

let%expect_test "int hash is not ident" [@tags "64-bits-only"] =
  print_s [%message "hash of 10"
                      (Int.hash 10 : int) ];
  [%expect {| ("hash of 10" ("Int.hash 10" 1_579_120_067_278_557_813)) |}];
;;
