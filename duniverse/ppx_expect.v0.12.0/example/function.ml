let%expect_test _ =
  let f output =
    print_string output;
    [%expect {| hello world |}]
  in
  f "hello world";
  f "hello world";
;;

let%expect_test _ =
  let f () =
    print_string "";
    [%expect {| |}]
  in
  f ();
  f ();
;;
