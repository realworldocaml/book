open! Import
open! Exn

let%expect_test "[create_s]" =
  print_s [%sexp (create_s [%message "foo"] : t)];
  [%expect {|
    foo |}];
  print_s [%sexp (create_s [%message "foo" "bar"] : t)];
  [%expect {|
    (foo bar) |}];
  let sexp = [%message "foo"] in
  print_s [%sexp (phys_equal sexp (sexp_of_t (create_s sexp)) : bool)];
  [%expect {|
    true |}];
;;

let%test _ = not (does_raise Fn.ignore)
let%test _ = does_raise (fun () -> failwith "foo")
