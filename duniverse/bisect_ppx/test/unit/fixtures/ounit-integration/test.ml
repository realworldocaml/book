open OUnit2

let (===) = assert_equal ~printer: String.escaped

let test_foo _ =
  "foo" === "foo"

let test_bar _ =
  "bar" === "bar"

let test_baz _ =
  "baz" === "baz"

let test_qux _ =
  "qux" === "qux"

let test_suite =
  "example" >::: [
    "foo" >:: test_foo;
    "bar" >:: test_bar;
    "baz" >:: test_baz;
    "qux" >:: test_qux;
  ]


let suite =
  "all" >::: [
    test_suite;
  ];;

run_test_tt_main suite
