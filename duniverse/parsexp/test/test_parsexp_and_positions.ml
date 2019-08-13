open Import
open Parsexp

let test input =
  Single_just_positions.parse_string input
  |> Result.map ~f:Positions.to_list
  |> [%sexp_of: (Positions.pos list, Parse_error.t) Result.t]
  |> print_s
;;

let test_many input =
  Many_just_positions.parse_string input
  |> Result.map ~f:Positions.to_list
  |> [%sexp_of: (Positions.pos list, Parse_error.t) Result.t]
  |> print_s
;;

let%expect_test "single" =
  test "(1 2 3)";
  [%expect {|
    (Ok (
      ((line 1) (col 0) (offset 0))
      ((line 1) (col 1) (offset 1))
      ((line 1) (col 1) (offset 1))
      ((line 1) (col 3) (offset 3))
      ((line 1) (col 3) (offset 3))
      ((line 1) (col 5) (offset 5))
      ((line 1) (col 5) (offset 5))
      ((line 1) (col 6) (offset 6))))
  |}];
  test "; plop\natom";
  [%expect{|
    (Ok (
      ((line 2) (col 0) (offset 7))
      ((line 2) (col 3) (offset 10))))
  |}];
  test "(1 (2 3)(4 5))";
  [%expect {|
    (Ok (
      ((line 1) (col 0)  (offset 0))
      ((line 1) (col 1)  (offset 1))
      ((line 1) (col 1)  (offset 1))
      ((line 1) (col 3)  (offset 3))
      ((line 1) (col 4)  (offset 4))
      ((line 1) (col 4)  (offset 4))
      ((line 1) (col 6)  (offset 6))
      ((line 1) (col 6)  (offset 6))
      ((line 1) (col 7)  (offset 7))
      ((line 1) (col 8)  (offset 8))
      ((line 1) (col 9)  (offset 9))
      ((line 1) (col 9)  (offset 9))
      ((line 1) (col 11) (offset 11))
      ((line 1) (col 11) (offset 11))
      ((line 1) (col 12) (offset 12))
      ((line 1) (col 13) (offset 13)))) |}]
;;

let%expect_test "many" =
  test_many "(1 2) (3)";
  [%expect {|
    (Ok (
      ((line 1) (col 0) (offset 0))
      ((line 1) (col 1) (offset 1))
      ((line 1) (col 1) (offset 1))
      ((line 1) (col 3) (offset 3))
      ((line 1) (col 3) (offset 3))
      ((line 1) (col 4) (offset 4))
      ((line 1) (col 6) (offset 6))
      ((line 1) (col 7) (offset 7))
      ((line 1) (col 7) (offset 7))
      ((line 1) (col 8) (offset 8))))
  |}];
  test_many "(1 2 3)\nhello";
  [%expect{|
    (Ok (
      ((line 1) (col 0) (offset 0))
      ((line 1) (col 1) (offset 1))
      ((line 1) (col 1) (offset 1))
      ((line 1) (col 3) (offset 3))
      ((line 1) (col 3) (offset 3))
      ((line 1) (col 5) (offset 5))
      ((line 1) (col 5) (offset 5))
      ((line 1) (col 6) (offset 6))
      ((line 2) (col 0) (offset 8))
      ((line 2) (col 4) (offset 12))))
  |}];
  test_many "(1 2)(3)";
  [%expect {|
    (Ok (
      ((line 1) (col 0) (offset 0))
      ((line 1) (col 1) (offset 1))
      ((line 1) (col 1) (offset 1))
      ((line 1) (col 3) (offset 3))
      ((line 1) (col 3) (offset 3))
      ((line 1) (col 4) (offset 4))
      ((line 1) (col 5) (offset 5))
      ((line 1) (col 6) (offset 6))
      ((line 1) (col 6) (offset 6))
      ((line 1) (col 7) (offset 7)))) |}]
;;

