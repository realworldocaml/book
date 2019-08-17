
open! Import
open! Int32

let%expect_test "hash coherence" =
  check_int_hash_coherence [%here] (module Int32);
  [%expect {| |}];
;;
