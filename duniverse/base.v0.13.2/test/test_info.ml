open! Import
open! Info

let%expect_test _ =
  print_endline (to_string_hum (of_exn (Failure "foo")));
  [%expect {| (Failure foo) |}]
;;

let%expect_test _ =
  print_endline (to_string_hum (tag (of_string "b") ~tag:"a"));
  [%expect {| (a b) |}]
;;

let%expect_test _ =
  print_endline (to_string_hum (of_list (List.map ~f:of_string [ "a"; "b"; "c" ])));
  [%expect {| (a b c) |}]
;;

let%expect_test _ =
  print_endline
    (to_string_hum (tag_s ~tag:[%message "tag"] (create_s [%message "info"])));
  [%expect {| (tag info) |}]
;;

let of_strings strings = of_list (List.map ~f:of_string strings)

let nested =
  of_list
    (List.map ~f:of_strings [ [ "a"; "b"; "c" ]; [ "d"; "e"; "f" ]; [ "g"; "h"; "i" ] ])
;;

let%expect_test _ =
  print_endline (to_string_hum nested);
  [%expect {| (a b c d e f g h i) |}]
;;

let%expect_test _ =
  require_equal
    [%here]
    (module Sexp)
    (sexp_of_t nested)
    (sexp_of_t (of_strings [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i" ]));
  [%expect {| |}]
;;

let%expect_test _ =
  match to_exn (of_exn (Failure "foo")) with
  | Failure "foo" -> ()
  | exn -> raise_s [%sexp { got = (exn : exn); expected = Failure "foo" }]
;;

let round t =
  let sexp = sexp_of_t t in
  require [%here] (Sexp.( = ) sexp (sexp_of_t (t_of_sexp sexp)))
;;

let%expect_test "non-empty tag" =
  tag_arg (of_string "hello") "tag" 13 [%sexp_of: int] |> sexp_of_t |> print_s;
  [%expect {| (tag 13 hello) |}]
;;

let%expect_test "empty tag" =
  tag_arg (of_string "hello") "" 13 [%sexp_of: int] |> sexp_of_t |> print_s;
  [%expect {| (13 hello) |}]
;;

let%expect_test _ = round (of_string "hello")
let%expect_test _ = round (of_thunk (fun () -> "hello"))
let%expect_test _ = round (create "tag" 13 [%sexp_of: int])
let%expect_test _ = round (tag (of_string "hello") ~tag:"tag")
let%expect_test _ = round (tag_arg (of_string "hello") "tag" 13 [%sexp_of: int])
let%expect_test _ = round (tag_arg (of_string "hello") "" 13 [%sexp_of: int])
let%expect_test _ = round (of_list [ of_string "hello"; of_string "goodbye" ])

let%expect_test _ =
  round (t_of_sexp (Sexplib.Sexp.of_string "((random sexp 1)(b 2)((c (1 2 3))))"))
;;

let%expect_test _ =
  require_equal [%here] (module String) (to_string_hum (of_string "a\nb")) "a\nb";
  [%expect {| |}]
;;
