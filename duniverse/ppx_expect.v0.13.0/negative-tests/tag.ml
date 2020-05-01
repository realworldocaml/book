open! Core

let%expect_test _ =
  (* Correction should include a string tag *)
  print_string "{|String tag required|}";
  [%expect {||}]
;;

let%expect_test _ =
  (* The correction should use the same string-kind (normal,quoted) as the [%expect] *)
  print_string "foo\\bar";
  [%expect {||}];
  print_string "hey\\ho";
  [%expect_exact ""];
  print_string {|
    Foo
    "bar baz"|};
  [%expect.unreachable]
;;
