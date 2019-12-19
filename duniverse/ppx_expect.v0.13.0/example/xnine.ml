(*
   Demonstate use of [%expect_exact]
   to match a single line of text with 0|1|2 leading & trailing NLs.

   Starting with..

   {[
     let%expect_test _ =
       print_string "hello";          [%expect_exact ""];
       print_string "hello\n";        [%expect_exact ""];
       print_string "hello\n\n";      [%expect_exact ""];
       print_string "\nhello";        [%expect_exact ""];
       print_string "\nhello\n";      [%expect_exact ""];
       print_string "\nhello\n\n";    [%expect_exact ""];
       print_string "\n\nhello";      [%expect_exact ""];
       print_string "\n\nhello\n";    [%expect_exact ""];
       print_string "\n\nhello\n\n";  [%expect_exact ""];
     ;;
   ]}

   Generate with [cp xnine.ml.corrected xnine.ml] the following [%expect_exact]... *)

let%expect_test _ =
  print_string "hello";
  [%expect_exact {|hello|}];
  print_string "hello\n";
  [%expect_exact "hello\n"];
  print_string "hello\n\n";
  [%expect_exact "hello\n\n"];
  print_string "\nhello";
  [%expect_exact "\nhello"];
  print_string "\nhello\n";
  [%expect_exact "\nhello\n"];
  print_string "\nhello\n\n";
  [%expect_exact "\nhello\n\n"];
  print_string "\n\nhello";
  [%expect_exact "\n\nhello"];
  print_string "\n\nhello\n";
  [%expect_exact "\n\nhello\n"];
  print_string "\n\nhello\n\n";
  [%expect_exact "\n\nhello\n\n"]
;;
