open! Base
open Stdio
open Float

let%expect_test "iround_nearest_exn noalloc" =
  let t = Sys.opaque_identity 205.414 in
  Expect_test_helpers_core.require_no_allocation [%here] (fun () -> iround_nearest_exn t)
  |> printf "%d\n";
  [%expect {| 205 |}]
;;
