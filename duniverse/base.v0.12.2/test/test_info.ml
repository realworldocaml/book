open! Import
open! Info

let%test_unit _ =
  [%test_result: string] (to_string_hum (of_exn (Failure "foo")))
    ~expect:"(Failure foo)"
;;

let%test_unit _ =
  [%test_result: string] (to_string_hum (tag (of_string "b") ~tag:"a"))
    ~expect:"(a b)"
;;

let%test_unit _ =
  [%test_result: string]
    (to_string_hum (of_list (List.map ~f:of_string [ "a"; "b"; "c" ])))
    ~expect:"(a b c)"
;;

let of_strings strings = of_list (List.map ~f:of_string strings)

let nested =
  of_list
    (List.map ~f:of_strings
       [ [ "a"; "b"; "c" ]
       ; [ "d"; "e"; "f" ]
       ; [ "g"; "h"; "i" ]
       ])
;;

let%test_unit _ =
  [%test_result: string] (to_string_hum nested) ~expect:"(a b c d e f g h i)"
;;

let%test_unit _ =
  [%test_result: Sexp.t] (sexp_of_t nested)
    ~expect:(sexp_of_t (of_strings [ "a"; "b"; "c"
                                   ; "d"; "e"; "f"
                                   ; "g"; "h"; "i" ]))
;;

let%test_unit _ =
  match to_exn (of_exn (Failure "foo")) with
  | Failure "foo" -> ()
  | exn -> raise_s [%sexp { got : exn = exn; expected = Failure "foo" }]
;;

let round t =
  let sexp = sexp_of_t t in
  Sexp.(=) sexp (sexp_of_t (t_of_sexp sexp))
;;

let%test _ = round (of_string "hello")
let%test _ = round (of_thunk (fun () -> "hello"))
let%test _ = round (create "tag" 13 [%sexp_of: int])
let%test _ = round (tag (of_string "hello") ~tag:"tag")
let%test _ = round (tag_arg (of_string "hello") "tag" 13
                      [%sexp_of: int])
let%test _ = round (of_list [ of_string "hello"; of_string "goodbye" ])
let%test _ = round (t_of_sexp (Sexplib.Sexp.of_string "((random sexp 1)(b 2)((c (1 2 3))))"))

let%test _ = String.equal (to_string_hum (of_string "a\nb")) "a\nb"
