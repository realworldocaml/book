
open! Core

(* The generated expectation should follow user formatting when present, otherwise it
   should follow a sensible default *)


(* Single line actual.. *)

let%expect_test _ =
  print_string "hello";
  [%expect {||}]


let%expect_test _ =
  print_string "hello";
  [%expect {|
  |}]

let%expect_test _ =
  print_string "hello";
  [%expect {|
           |}]

let%expect_test _ =
  print_string "hello";
  [%expect {|  WRONG
           |}]

let%expect_test _ =
  print_string "hello";
  [%expect {|  WRONG
           |}]

let%expect_test _ =
  print_string "hello";
  [%expect {|
  WRONG |}]

let%expect_test _ =
  print_string "hello";
  [%expect {|
       WRONG
  |}]


(* Multi line actual... *)

let%expect_test _ =
  print_string "one1\ntwo";
  [%expect {||}]

let%expect_test _ =
  print_string "one2\ntwo";
  [%expect {|
  |}]

let%expect_test _ =
  print_string "one3\ntwo";
  [%expect {|
           |}]

let%expect_test _ =
  print_string "one4\ntwo";
  [%expect {|  WRONG
           |}]

let%expect_test _ =
  print_string "one5\ntwo";
  [%expect {|
  WRONG |}]

let%expect_test _ =
  print_string "one6\ntwo";
  [%expect {|
       WRONG
  |}]

let%expect_test _ =
  print_string "one8\ntwo";
  [%expect {|
  WRONG
  THING |}]

let%expect_test _ =
  print_string "one9\ntwo";
  [%expect {|
       WRONG
       THING
  |}]

let%expect_test _ =
  print_string "one10\ntwo";
  [%expect {|
       WRONG
          THING
  |}]

let%expect_test _ =
  print_string "one11\ntwo";
  [%expect {|
          WRONG
       THING
  |}]
