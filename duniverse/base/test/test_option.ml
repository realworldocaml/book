open! Import
open! Option

let f = ( + )

let%test _ = [%compare.equal: int t] (merge None None ~f) None
let%test _ = [%compare.equal: int t] (merge (Some 3) None ~f) (Some 3)
let%test _ = [%compare.equal: int t] (merge None (Some 3) ~f) (Some 3)
let%test _ = [%compare.equal: int t] (merge (Some 1) (Some 3) ~f) (Some 4)

let%expect_test "[value_or_thunk]" =
  let default () =
    print_endline "THUNK!";
    0
  in
  let value_or_thunk = value_or_thunk ~default in
  let test t = print_s [%sexp (value_or_thunk t : int)] in
  (* trigger the thunk *)
  test None;
  [%expect {|
    THUNK!
    0 |}];
  (* same value, no trigger *)
  test (Some 0);
  [%expect {| 0 |}];
  (* different value *)
  test (Some 1);
  [%expect {| 1 |}];
  (* trigger the thunk again: no memoization *)
  test None;
  [%expect {|
    THUNK!
    0 |}]
;;
