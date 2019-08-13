open! Import
open! Sexp_pretty

let normalized_sexp t =
  let rec of_t = function
    | Normalize.Sexp sexp -> Some (of_sexp sexp)
    | Normalize.Comment _ -> None
  and of_sexp = function
    | Normalize.Atom str -> Sexp.Atom str
    | Normalize.List ts  -> Sexp.List (of_t_list ts)
  and of_t_list ts = List.filter_map ts ~f:of_t
  in
  match of_t t with
  | Some sexp -> sexp
  | None -> assert false
;;

let normalize conf sexp =
  sexp
  |> sexp_to_sexp_or_comment
  |> Normalize.of_sexp_or_comment conf
  |> normalized_sexp
;;

let conf = { Config.default with Config.atom_printing = Interpreted }

let test ~input:input_sexp sexp =
  [%test_result: Sexp.t] ~expect:sexp (normalize conf input_sexp)
;;

let%test_unit _ =
  let atom = Sexp.Atom "Not connected to oculus monitor" in
  test ~input:atom atom
;;

let%test_unit _ =
  test ~input:(Sexp.Atom "Not connected to oculus monitor (connect to monitor please)")
    (List [ Atom "Not connected to oculus monitor"
          ; Sexp.of_string "(connect to monitor please)"
          ])
;;

let%test_unit _ =
  test ~input:(Sexp.Atom "Not connected (not that you have to) to oculus monitor")
    (List [ Atom "Not connected"
          ; Sexp.of_string "(not that you have to)"
          ; Atom "to oculus monitor"
          ])
;;

let%test_unit _ =
  let sexp = Sexp.of_string "(this is a (bona fide) sexp)" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.of_string "(issue (error_fields (message \"A message\")(int 5)))" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.of_string "(message \"A message\")" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.Atom "\"   space   \"" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.Atom "   space   " in
  test ~input:sexp sexp
;;

(* And a couple of tests without atom interpretation *)

let conf = { Config.default with Config.atom_printing = Escaped }

let test ~input:input_sexp sexp =
  [%test_result: Sexp.t] ~expect:sexp (normalize conf input_sexp)
;;

let%test_unit _ =
  let atom = Sexp.Atom "Not connected to oculus monitor" in
  test ~input:atom atom
;;

let%test_unit _ =
  let sexp = Sexp.Atom "(this is a (bona fide) sexp)" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.Atom "(message \"A message\")" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.Atom "\"   space   \"" in
  test ~input:sexp sexp
;;

let%test_unit _ =
  let sexp = Sexp.Atom "   space   " in
  test ~input:sexp sexp
;;

let%expect_test "long atoms with newlines are hard to read" =
  let s = String.concat ~sep:"\n" (List.init 10 ~f:Int.to_string) in
  print_endline (pretty_string Config.(update default ~color:false) (Atom s));
  [%expect {| "0\n1\n2\n3\n4\n5\n6\n7\n8\n9" |}];
  print_endline (Sexp.to_string_hum (Atom s));
  [%expect {|
     "0\
    \n1\
    \n2\
    \n3\
    \n4\
    \n5\
    \n6\
    \n7\
    \n8\
    \n9" |}]
;;
