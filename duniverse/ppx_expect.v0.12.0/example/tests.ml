open Core

(* We may use other syntax extensions when writing expect tests. *)
type t = int list [@@deriving sexp_of]

let pr s = Printf.printf "%s\n" s


let%expect_test "foo" =
  pr "line1";
  pr (Sexp.to_string (sexp_of_t [1;2;3]));
  [%expect {|
    line1
    (1 2 3)
  |}]
;;

let%expect_test _ =
  pr "line2";
  pr "start - blah - stop";
  pr "line3";

  [%expect {|
    line2
    start .* stop (regexp)
    line3
  |}]
;;

let%expect_test _ =
  print_string "hello, world!";
  [%expect "hello, world!"]
;;

let%expect_test _ =
  print_string "hello, world!";
  [%expect_exact {|hello, world!|}]
;;

let%expect_test _ =
  print_string "I need |}weird escaping";
  [%expect {xxx| I need |}weird escaping |xxx}]
;;

let%expect_test _ =
  print_string "hello world";   [%expect {| \(hello\|goodbye\) world (regexp) |}];
  print_string "goodbye world"; [%expect {| \(hello\|goodbye\) world (regexp) |}];

  print_string "a";            [%expect {| a |}];
  print_string "a";            [%expect {| [a] (regexp) |}];
  print_string "[a]";          [%expect {| [a] |}];
  print_string "[a] (regexp)"; [%expect {| [a] (regexp) (literal) |}];

  print_string "axxa";       [%expect {| axxa |}];
  print_string "axxa";       [%expect {| a*a  (glob) |}];
  print_string "axxa";       [%expect {| a??a (glob) |}];
  print_string "axxa";       [%expect {| a.*a (regexp) |}];
  print_string "a*a";        [%expect {| a*a |}];
  print_string "a*a (glob)"; [%expect {| a*a (glob) (literal) |}];
;;

let%expect_test _ =
  print_endline "foogle";
  print_endline "magoo";
  [%expect {|
    foo*e             (glob)
    \(mister\|magoo\) (regexp)
  |}]
;;
