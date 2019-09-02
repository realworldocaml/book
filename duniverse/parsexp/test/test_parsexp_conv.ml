open Import
open Parsexp

type 'a conv_result = ('a, Conv_error.t) Result.t [@@deriving sexp_of]

let test a_of_sexp input =
  match Conv_single.parse_string input a_of_sexp with
  | Ok _ -> ()
  | Error error ->
    Conv_error.report Caml.Format.std_formatter error ~filename:"<string>"
;;

let test_many a_of_sexp input =
  match Conv_many.parse_string input a_of_sexp with
  | Ok _ -> ()
  | Error error ->
    Conv_error.report Caml.Format.std_formatter error ~filename:"<string>"
;;

let%expect_test "parsing errors" =
  (* For the following tests, you can check that the reported character positions match
     the positions reported by emacs when you move the cursor over the faulty
     s-expressions. *)
  test [%of_sexp: int list] {|
(1 2 3|};
  [%expect {|
    File "<string>", line 2, character 6:
    Error: s-expression parsing error;
    unclosed parentheses at end of input
  |}];
  test [%of_sexp: int list] {|
(1 2 "abc)|};
  [%expect {|
    File "<string>", line 2, character 10:
    Error: s-expression parsing error;
    unterminated quoted string
  |}];
  test_many [%of_sexp: int list] {|
(1 2 3)
"a|};
  [%expect {|
    File "<string>", line 3, character 2:
    Error: s-expression parsing error;
    unterminated quoted string
  |}];
;;

let%expect_test "conversion errors" =
  (* For the following tests, you can check that the reported character positions match
     the positions reported by emacs when you move the cursor over the faulty
     s-expressions. *)
  test [%of_sexp: int list] {|
(1 2 3 abc 4 5 6)
|};
  [%expect {|
    File "<string>", line 2, characters 7-10:
    Error: s-expression conversion error;
    exception (Failure "int_of_sexp: (Failure int_of_string)")
  |}];
  test [%of_sexp: int list] {|
(1 2 (1 2 3))
|};
  [%expect {|
    File "<string>", line 2, characters 5-12:
    Error: s-expression conversion error;
    exception (Failure "int_of_sexp: atom needed")
  |}];
  test [%of_sexp: int list] {|
(1 2 (1
2
3))
|};
  [%expect {|
    File "<string>", line 2, characters 5-12:
    Error: s-expression conversion error;
    exception (Failure "int_of_sexp: atom needed")
  |}];
  test_many [%of_sexp: int list] {|
(1 2 3)
(a)
|};
  [%expect {|
    File "<string>", line 3, characters 1-2:
    Error: s-expression conversion error;
    exception (Failure "int_of_sexp: (Failure int_of_string)")
  |}];
;;
