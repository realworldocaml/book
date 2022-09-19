open! Base
open Expect_test_helpers_core

let%expect_test "is_prefix does not allocate" =
  let list = Sys.opaque_identity [ 1; 2; 3 ] in
  let prefix = Sys.opaque_identity [ 1; 2 ] in
  let equal = Int.equal in
  let (_ : bool) =
    require_no_allocation [%here] (fun () -> List.is_prefix list ~equal ~prefix)
  in
  [%expect {| |}]
;;

let%expect_test "is_suffix does not allocate" =
  let list = Sys.opaque_identity [ 1; 2; 3 ] in
  let suffix = Sys.opaque_identity [ 2; 3 ] in
  let equal = Int.equal in
  let (_ : bool) =
    require_no_allocation [%here] (fun () -> List.is_suffix list ~equal ~suffix)
  in
  [%expect {| |}]
;;
