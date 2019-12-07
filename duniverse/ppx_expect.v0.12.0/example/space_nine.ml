(*
   Demonstate use of [%expect]
   to match a single line of text with 0|1|2 leading & trailing NLs.
   The text begins with a single space.

   Starting with..

   {[
     let%expect_test _ =
       print_string " hello";          [%expect{||}];
       print_string " hello\n";        [%expect{||}];
       print_string " hello\n\n";      [%expect{||}];
       print_string "\n hello";        [%expect{||}];
       print_string "\n hello\n";      [%expect{||}];
       print_string "\n hello\n\n";    [%expect{||}];
       print_string "\n\n hello";      [%expect{||}];
       print_string "\n\n hello\n";    [%expect{||}];
       print_string "\n\n hello\n\n";  [%expect{||}];
     ;;
   ]}

   Generate with [cp space_nine.ml.corrected space_nine.ml] the following [%expect]... *)

let%expect_test _ =
  print_string " hello";          [%expect_exact " hello"];
  print_string " hello\n";        [%expect_exact " hello
"];
  print_string " hello\n\n";      [%expect_exact " hello

"];
  print_string "\n hello";        [%expect_exact "
 hello"];
  print_string "\n hello\n";      [%expect_exact "
 hello
"];
  print_string "\n hello\n\n";    [%expect_exact "
 hello

"];
  print_string "\n\n hello";      [%expect_exact "

 hello"];
  print_string "\n\n hello\n";    [%expect_exact "

 hello
"];
  print_string "\n\n hello\n\n";  [%expect_exact "

 hello

"]
;;
