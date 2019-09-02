
(* Hydra doesn't like .ml files containing tab chars. So such examples need to go here *)

let%expect_test _ =
  print_string "I have 8 spaces before me";
  [%expect {|
          I have 8 spaces before me|}];

  print_string "I have a tab char before me";
  [%expect {|
    I have a tab char before me|}]
