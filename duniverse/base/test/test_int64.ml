open! Import
open! Int64

let%expect_test "hash coherence" =
  check_int_hash_coherence [%here] (module Int64);
  [%expect {| |}];
;;
