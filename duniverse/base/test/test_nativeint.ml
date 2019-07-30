open! Import
open! Nativeint

let%expect_test "hash coherence" =
  check_int_hash_coherence [%here] (module Nativeint);
  [%expect {| |}];
;;
