open! Base
open! Import

let errors =
  [ Error.of_string "ABC"
  ; Error.tag ~tag:"DEF" (Error.of_thunk (fun () -> "GHI"))
  ; Error.create_s ([%message "foo" ~bar:(31:int)])
  ]

let%expect_test _ =
  List.iter errors ~f:(fun error -> show_raise (fun () -> Error.raise error));
  [%expect {|
    (raised ABC)
    (raised (DEF GHI))
    (raised (foo (bar 31))) |}]

let%expect_test _ =
  List.iter errors ~f:(fun error ->
    show_raise (fun () -> Error.raise_s [%sexp (error : Error.t)]));
  [%expect {|
    (raised ABC)
    (raised (DEF GHI))
    (raised (foo (bar 31))) |}]
