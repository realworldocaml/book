
let%expect_test _ =
  print_string " Be more";
  [%expect {| Be more |}];
  print_string "\nflexible\n";
  [%expect {| flexible |}]
