open! Core

let%expect_test _ =
  (* A literal match shouldn't succeed here! *)
  print_string "[a] (regexp)";
  [%expect {| [a] (regexp) |}];

  (* Only mismatched lines are replaced *)
  print_endline
    "hello world";
  print_endline "a";
  [%expect {|
    goodbye.*  (regexp)
    [a]        (regexp)
  |}]
