open Core
module Sh = Shell

let%expect_test "run_one_line" =
  printf !"%{sexp:string Or_error.t}\n" (Sh.sh_one_line "echo 'hi there'");
  [%expect {| (Ok "hi there") |}];

  printf !"%{sexp:string Or_error.t}\n" (Sh.sh_one_line "true");
  [%expect {| (Error "expected one line, got empty output") |}];

  printf !"%{sexp:string Or_error.t}\n" (Sh.sh_one_line "echo hi; echo there");
  [%expect {|
    (Error
     ("One line expected, got at least two lines of output" (first_line hi)
      (second_line there))) |}];

  printf !"%{sexp:string Or_error.t}\n" (Sh.sh_one_line "yes yes");
  [%expect {|
    (Error
     ("One line expected, got at least two lines of output" (first_line yes)
      (second_line yes))) |}];
;;
