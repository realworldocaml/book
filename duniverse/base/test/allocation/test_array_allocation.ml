open! Base
open Expect_test_helpers_core

let%expect_test "Array.sort [||] only allocates when computing bounds" =
  require_allocation_does_not_exceed (Minor_words 3) [%here] (fun () ->
    Array.sort ~compare:Int.compare [||]);
  [%expect {||}]
;;

let%expect_test "Array.sort [| 5; 2; 3; 4; 1 |] only allocates when computing bounds" =
  let arr = [| 5; 2; 3; 4; 1 |] in
  require_allocation_does_not_exceed (Minor_words 3) [%here] (fun () ->
    Array.sort ~compare:Int.compare arr);
  [%expect {||}]
;;

let%test "equal does not allocate" =
  let arr1 = [| 1; 2; 3; 4 |] in
  let arr2 = [| 1; 2; 4; 3 |] in
  require_no_allocation [%here] (fun () -> not (Array.equal Int.equal arr1 arr2))
;;

let%test "foldi does not allocate" =
  let arr = [| 1; 2; 3; 4 |] in
  let f i x y = i + x + y in
  require_no_allocation [%here] (fun () -> 16 = Array.foldi ~init:0 ~f arr)
;;
