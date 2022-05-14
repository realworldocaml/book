open! Core
open! Async

let trials = 5
let examples = [ "Example 1"; "Example 2"; "Example 3" ]
let generator = Quickcheck.Generator.return "Generated value"

let%expect_test "[~examples] are tested first" =
  let%bind () =
    Async_quickcheck.async_test ~trials ~examples generator ~f:(fun string ->
      print_endline string;
      return ())
  in
  [%expect
    {|
    Example 1
    Example 2
    Example 3
    Generated value
    Generated value
    Generated value
    Generated value
    Generated value |}];
  return ()
;;

let%expect_test "[test] and [async_test] handle [~examples] the same" =
  let%bind () =
    Async_quickcheck.async_test ~trials ~examples generator ~f:(fun string ->
      print_endline string;
      return ())
  in
  let async_test_output = Expect_test_helpers_base.expect_test_output [%here] in
  Async_quickcheck.test ~trials ~examples generator ~f:print_endline;
  let test_output = Expect_test_helpers_base.expect_test_output [%here] in
  Expect_test_helpers_base.require_equal
    [%here]
    (module String)
    async_test_output
    test_output;
  return ()
;;
