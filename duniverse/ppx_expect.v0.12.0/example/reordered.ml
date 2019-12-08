let%expect_test _ =
  let f () =
    print_string "bar";
    [%expect {| bar |}]
  in

  print_string "foo";
  [%expect {| foo |}];

  f ();
