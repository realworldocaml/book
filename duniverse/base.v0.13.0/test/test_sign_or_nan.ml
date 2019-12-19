open! Import
open! Sign_or_nan

let%test "of_int" = of_int 37 = Pos && of_int (-22) = Neg && of_int 0 = Zero

let%expect_test ("hash coherence"[@tags "64-bits-only"]) =
  check_hash_coherence [%here] (module Sign_or_nan) all;
  [%expect {| |}]
;;
